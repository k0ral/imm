{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Feed data structures.
module Imm.Feed (
  -- * Types
  FeedLocation (..),
  UID,
  FeedQuery (..),
  FeedDefinition (..),
  FeedItem (..),
  Author (..),

  -- * Parsers
  parseFeed,
  feedC,
  parseFeedItem,

  -- * Utilities
  getMainLink,
  areSameItem,
) where

-- {{{ Imports
import Conduit
import Control.Exception.Safe
import Data.Aeson.Extended
import Data.Text as Text (null)
import Data.Time
import Data.XML.Types
import Imm.Link
import Imm.Pretty
import Refined
import Safe
import Text.Atom.Conduit.Parse
import Text.Atom.Types
import Text.RSS.Conduit.Parse
import Text.RSS.Extensions.Content
import Text.RSS.Extensions.DublinCore
import Text.RSS.Types
import Text.RSS1.Conduit.Parse
import Text.XML.Stream.Parse as XML hiding (content)
import URI.ByteString.Extended

-- }}}

-- | Feed location identifies a feed. It is either:
-- - the feed URI
-- - a webpage URI that refers to the feed through an alternate link, in which case an optional feed title can be provided to disambiguate multiple such links
data FeedLocation = FeedLocation URI Text
  deriving (Eq, Generic, Ord, Show, Typeable)

instance Pretty FeedLocation where
  pretty (FeedLocation uri title) =
    prettyURI uri
      <> if Text.null title then mempty else space <> brackets (pretty title)

instance ToJSON FeedLocation where
  toJSON (FeedLocation uri title) =
    object
      [ "uri" .= toJSON (JsonURI uri)
      , "title" .= title
      ]

instance FromJSON FeedLocation where
  parseJSON = withObject "FeedLocation" $ \v → do
    FeedLocation <$> (_unwrapURI <$> (v .: "uri")) <*> (v .: "title")

-- | Database identifier for a feed
type UID = Int

-- | A query describes a set of feeds through some criteria.
data FeedQuery = QueryByUID UID | QueryAll
  deriving (Eq, Ord, Read, Show, Typeable)

instance Pretty FeedQuery where
  pretty QueryAll = "All subscribed feeds"
  pretty (QueryByUID k) = "Feed" <+> pretty k

newtype FeedDefinition = FeedDefinition
  { _feedTitle ∷ Text
  }
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

feedDefinitionOptions ∷ Options
feedDefinitionOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_feed")
    , omitNothingFields = True
    }

instance ToJSON FeedDefinition where
  toJSON = genericToJSON feedDefinitionOptions
  toEncoding = genericToEncoding feedDefinitionOptions

instance FromJSON FeedDefinition where
  parseJSON = genericParseJSON feedDefinitionOptions

instance Pretty FeedDefinition where
  pretty definition = pretty $ _feedTitle definition

instance Pretty (PrettyName FeedDefinition) where
  pretty (PrettyName definition) = pretty $ _feedTitle definition

data Author = Author
  { _authorName ∷ Text
  , _authorEmail ∷ Text
  , _authorURI ∷ Maybe AnyURI
  }
  deriving (Eq, Generic, Ord, Show, Typeable)

authorOptions ∷ Options
authorOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_author")
    , omitNothingFields = True
    }

instance ToJSON Author where
  toJSON = genericToJSON authorOptions
  toEncoding = genericToEncoding authorOptions

instance FromJSON Author where
  parseJSON = genericParseJSON authorOptions

instance Pretty Author where
  pretty Author{..} = pretty _authorName <+> brackets (pretty _authorEmail)

data FeedItem = FeedItem
  { _itemDate ∷ Maybe UTCTime
  , _itemTitle ∷ Text
  , _itemContent ∷ Text
  , _itemLinks ∷ [Link]
  , _itemIdentifier ∷ Text
  , _itemAuthors ∷ [Author]
  }
  deriving (Eq, Generic, Ord, Show, Typeable)

feedItemOptions ∷ Options
feedItemOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_item")
    , omitNothingFields = True
    }

instance ToJSON FeedItem where
  toJSON = genericToJSON feedItemOptions
  toEncoding = genericToEncoding feedItemOptions

instance FromJSON FeedItem where
  parseJSON = genericParseJSON feedItemOptions

instance Pretty (PrettyName FeedItem) where
  pretty (PrettyName item) = pretty (_itemTitle item)

instance Pretty FeedItem where
  pretty FeedItem{..} = maybe "<unknown>" prettyTime _itemDate <+> pretty _itemTitle

parseFeed ∷ MonadCatch m ⇒ Text → m (FeedDefinition, [FeedItem])
parseFeed text = runConduit $ parseLBS def (encodeUtf8 text) .| XML.force "Invalid feed" feedC

parseAtomFeed ∷ AtomFeed → (FeedDefinition, [FeedItem])
parseAtomFeed feed = (definition, items)
 where
  definition = FeedDefinition (show $ prettyAtomText $ feedTitle feed)
  items = parseAtomItem <$> feedEntries feed

parseRssFeed ∷ RssDocument (ContentModule (DublinCoreModule NoExtensions)) → (FeedDefinition, [FeedItem])
parseRssFeed doc = (definition, items)
 where
  definition = FeedDefinition (channelTitle doc)
  items = parseRssItem <$> channelItems doc

-- | Conduit version of 'parseFeed'
feedC ∷ MonadCatch m ⇒ ConduitT Event o m (Maybe (FeedDefinition, [FeedItem]))
feedC = choose [atom, rss, rss1]
 where
  atom = fmap parseAtomFeed <$> atomFeed
  rss = fmap parseRssFeed <$> rssDocument
  rss1 = fmap parseRssFeed <$> rss1Document

parseFeedItem ∷ MonadCatch m ⇒ Text → m FeedItem
parseFeedItem text = runConduit $ parseLBS def (encodeUtf8 text) .| XML.force "Invalid feed element" (choose [fmap parseAtomItem <$> atomEntry, fmap parseRssItem <$> rssItem, fmap parseRssItem <$> rss1Item])

parseAtomItem ∷ AtomEntry → FeedItem
parseAtomItem entry = FeedItem date title content links identifier authors
 where
  date = Just $ entryUpdated entry
  title = show $ prettyAtomText $ entryTitle entry
  content = fromMaybe "<empty>" $ rawContent <|> summary
  rawContent = show . prettyAtomContent <$> entryContent entry
  summary = show . prettyAtomText <$> entrySummary entry
  links = parseLink <$> entryLinks entry
  parseLink link = Link (pure $ fromMaybe Alternate $ parseRelation $ linkRel link) (linkTitle link) (parseMediaType $ linkType link) (withAtomURI AnyURI $ linkHref link)
  identifier = entryId entry
  authors = [Author (unrefine name) email (withAtomURI AnyURI <$> uri) | AtomPerson name email uri ← entryAuthors entry]

parseRssItem ∷ RssItem (ContentModule (DublinCoreModule NoExtensions)) → FeedItem
parseRssItem item = FeedItem date title content links identifier authors
 where
  date = itemPubDate item <|> (item & itemExtensions & itemContentOther & itemDcMetaData & elementDate)
  title = itemTitle item
  content = if not (Text.null rawContent) then rawContent else itemDescription item
  rawContent = item & itemExtensions & itemContent
  links = [Link (Just Alternate) mempty mzero (withRssURI AnyURI uri) | uri ← maybeToList $ itemLink item]
  identifier = itemGuid item <&> prettyGuid & maybe mempty show
  authors = [Author (itemAuthor item) mempty mzero]

-- TODO: replace headMay with singleMay
getMainLink ∷ FeedItem → Maybe Link
getMainLink item = _itemLinks item & filter (\l → _linkRelation l == Just Alternate) & headMay

haveSameIdentifier ∷ FeedItem → FeedItem → Maybe Bool
haveSameIdentifier item1 item2 = case (_itemIdentifier item1, _itemIdentifier item2) of
  ("", "") → Nothing
  (a, b) → Just (a == b)

haveSameLink ∷ FeedItem → FeedItem → Maybe Bool
haveSameLink item1 item2 = case (getMainLink item1, getMainLink item2) of
  (Just a, Just b) → Just (a == b)
  (Nothing, Nothing) → Nothing
  _ → Just False

haveSameTitle ∷ FeedItem → FeedItem → Maybe Bool
haveSameTitle item1 item2 = case (_itemTitle item1, _itemTitle item2) of
  ("" ∷ Text, "" ∷ Text) → Nothing
  (a, b) → Just (a == b)

areSameItem ∷ FeedItem → FeedItem → Bool
areSameItem a b = fromMaybe (comparing _itemContent a b == EQ) $ haveSameIdentifier a b <|> haveSameLink a b <|> haveSameTitle a b
