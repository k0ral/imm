{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
-- | Helpers to manipulate feeds
module Imm.Feed where

-- {{{ Imports
import           Imm.Pretty

import           Conduit
import           Control.Exception.Safe
import           Data.Binary.Builder
import           Data.Text                      as Text (null)
import           Data.Time
import           Data.Type.Equality
import           Lens.Micro
import           Text.Atom.Conduit.Parse
import           Text.Atom.Conduit.Render
import           Text.Atom.Types
import           Text.RSS.Conduit.Parse
import           Text.RSS.Conduit.Render
import           Text.RSS.Extensions.Content
import           Text.RSS.Extensions.DublinCore
import           Text.RSS.Lens
import           Text.RSS.Types
import           Text.RSS1.Conduit.Parse
import           Text.XML.Stream.Parse          as XML hiding (content)
import           Text.XML.Stream.Render         hiding (content)
import           URI.ByteString
-- }}}

-- * Types

-- | Feed reference: either its URI, or its UID from database
data FeedRef = ByUID Int | ByURI URI
  deriving(Eq, Ord, Show)

instance Pretty FeedRef where
  pretty (ByUID n) = "feed" <+> pretty n
  pretty (ByURI u) = prettyURI u

data Feed = Rss (RssDocument '[ContentModule, DublinCoreModule]) | Atom AtomFeed
  deriving(Eq, Ord, Show)

data FeedElement = RssElement (RssItem '[ContentModule, DublinCoreModule]) | AtomElement AtomEntry
  deriving(Show)

instance Pretty (PrettyKey FeedElement) where
  pretty (PrettyKey element) = "element" <+> pretty (getTitle element)

instance Ord FeedElement where
  compare element1 element2 = compare (getId element1) (getId element2)
    <> compare (getLink element1) (getLink element2)
    <> compare (getTitle element1) (getTitle element2)
    <> compare (getContent element1) (getContent element2)

instance Eq FeedElement where
  element1 == element2 = compare element1 element2 == EQ


data FeedURI = forall a . FeedURI (URIRef a)

deriving instance Show FeedURI
instance Eq FeedURI where
  (FeedURI a) == (FeedURI b) = case sameURIType a b of
    Just Refl -> a == b
    _         -> False

instance Ord FeedURI where
  compare (FeedURI a) (FeedURI b) = case (a, b) of
    (URI{}, URI{})                 -> compare a b
    (RelativeRef{}, RelativeRef{}) -> compare a b
    (URI{}, RelativeRef{})         -> LT
    (RelativeRef{}, URI{})         -> GT

sameURIType :: URIRef a1 -> URIRef a2 -> Maybe (URIRef a1 :~: URIRef a2)
sameURIType a b = case (a, b) of
  (URI{}, URI{})                 -> Just Refl
  (RelativeRef{}, RelativeRef{}) -> Just Refl
  _                              -> Nothing


withFeedURI :: (forall a . URIRef a -> b) -> FeedURI -> b
withFeedURI f (FeedURI a) = f a


-- * Generic parsers/renderers

renderFeed :: Feed -> Text
renderFeed (Rss rss) = decodeUtf8 $ toLazyByteString $ runConduitPure $ renderRssDocument rss .| renderBuilder def .| foldC
renderFeed (Atom atom) = decodeUtf8 $ toLazyByteString $ runConduitPure $ renderAtomFeed atom .| renderBuilder def .| foldC

renderFeedElement :: FeedElement -> Text
renderFeedElement (RssElement item) = decodeUtf8 $ toLazyByteString $ runConduitPure $ renderRssItem item .| renderBuilder def .| foldC
renderFeedElement (AtomElement entry) = decodeUtf8 $ toLazyByteString $ runConduitPure $ renderAtomEntry entry .| renderBuilder def .| foldC

parseFeed :: MonadCatch m => Text -> m Feed
parseFeed text = runConduit $ parseLBS def (encodeUtf8 text) .| XML.force "Invalid feed" (choose [fmap Atom <$> atomFeed, fmap Rss <$> rssDocument, fmap Rss <$> rss1Document])

parseFeedElement :: MonadCatch m => Text -> m FeedElement
parseFeedElement text = runConduit $ parseLBS def (encodeUtf8 text) .| XML.force "Invalid feed element" (choose [fmap AtomElement <$> atomEntry, fmap RssElement <$> rssItem, fmap RssElement <$> rss1Item])


-- * Generic mutators

removeElements :: Feed -> Feed
removeElements (Rss rss)   = Rss $ rss { channelItems = mempty }
removeElements (Atom atom) = Atom $ atom { feedEntries = mempty }


-- * Generic getters

getFeedTitle :: Feed -> Text
getFeedTitle (Rss doc)   = channelTitle doc
getFeedTitle (Atom feed) = show $ prettyAtomText $ feedTitle feed

getElements :: Feed -> [FeedElement]
getElements (Rss doc)   = map RssElement $ channelItems doc
getElements (Atom feed) = map AtomElement $ feedEntries feed

getDate :: FeedElement -> Maybe UTCTime
getDate (RssElement item)   = itemPubDate item <|> elementDate (itemDcMetaData $ item ^. itemExtensionL)
getDate (AtomElement entry) = Just $ entryUpdated entry

getTitle :: FeedElement -> Text
getTitle (RssElement item)   = itemTitle item
getTitle (AtomElement entry) = show $ prettyAtomText $ entryTitle entry

getContent :: FeedElement -> Text
getContent (RssElement item) = if not (Text.null content) then content else itemDescription item where
  ContentItem content = item ^. itemExtensionL
getContent (AtomElement entry) = fromMaybe "<empty>" $ content <|> summary where
  content = show . prettyAtomContent <$> entryContent entry
  summary = show . prettyAtomText <$> entrySummary entry

getLink :: FeedElement -> Maybe FeedURI
getLink (RssElement item) = itemLink item <&> withRssURI FeedURI
getLink (AtomElement entry) = (alternateLink <|> defaultLink) <&> linkHref <&> withAtomURI FeedURI where
  links = entryLinks entry
  alternateLink = links & filter (\link -> linkRel link == "alternate") & nonEmpty <&> head
  defaultLink = links & filter (Text.null . linkRel) & nonEmpty <&> head

getId :: FeedElement -> Text
getId (RssElement item)   = itemGuid item <&> prettyGuid & maybe mempty show
getId (AtomElement entry) = entryId entry

-- * Misc

prettyElement :: FeedElement -> Doc a
prettyElement (RssElement item)   = prettyItem item
prettyElement (AtomElement entry) = prettyEntry entry
