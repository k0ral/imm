{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Helpers to manipulate feeds
module Imm.Feed where

-- {{{ Imports
import           Imm.Prelude
import           Imm.Pretty

import           Data.Hashable
import           Data.Time
import           Lens.Micro
import           Text.Atom.Types
import           Text.RSS.Extensions.Content
import           Text.RSS.Extensions.DublinCore
import           Text.RSS.Lens
import           Text.RSS.Types
import           URI.ByteString
-- }}}

-- * Types

-- | Feed reference: either its URI, or its UID from database
data FeedRef = ByUID Int | ByURI URI
  deriving(Eq, Show)

instance Pretty FeedRef where
  pretty (ByUID n) = text "feed" <+> text (show n)
  pretty (ByURI u) = prettyURI u

data Feed = Rss (RssDocument '[ContentModule, DublinCoreModule]) | Atom AtomFeed
  deriving(Eq, Show)

data FeedElement = RssElement (RssItem '[ContentModule, DublinCoreModule]) | AtomElement AtomEntry
  deriving(Eq, Show)


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
getContent (RssElement item) = if not (null content) then content else itemDescription item where
  ContentItem content = item ^. itemExtensionL
getContent (AtomElement entry) = fromMaybe "<empty>" $ content <|> summary where
  content = show . prettyAtomContent <$> entryContent entry
  summary = show . prettyAtomText <$> entrySummary entry


getHashes :: FeedElement -> [Int]
getHashes (RssElement item) = map (hash . (show :: Doc -> String) . prettyGuid) (maybeToList $ itemGuid item)
  <> map ((hash :: String -> Int) . show . withRssURI prettyURI) (maybeToList $ itemLink item)
  <> [hash $ itemTitle item]
  <> [hash $ itemDescription item]
getHashes (AtomElement entry) = [hash $ entryId entry, (hash :: String -> Int) $ show $ prettyAtomText $ entryTitle entry]


-- * Misc

prettyElement :: FeedElement -> Doc
prettyElement (RssElement item)   = prettyItem item
prettyElement (AtomElement entry) = prettyEntry entry
