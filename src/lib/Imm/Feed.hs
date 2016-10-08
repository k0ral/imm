{-# LANGUAGE NoImplicitPrelude #-}
-- | Helpers to manipulate feeds
module Imm.Feed where

-- {{{ Imports
import           Imm.Prelude
import           Imm.Pretty

import           Data.Hashable
import           Data.NonNull
import           Data.Time

import           Text.Atom.Types
import           Text.RSS.Types

import           URI.ByteString
-- }}}

-- * Types

-- | Feed reference: either its URI, or its UID from database
newtype FeedRef = FeedRef (Either Int URI)
  deriving(Eq, Show)

instance Pretty FeedRef where
  pretty (FeedRef (Left n))  = text "feed" <+> text (show n)
  pretty (FeedRef (Right u)) = prettyURI u

data Feed = Rss RssDocument | Atom AtomFeed
  deriving(Eq, Show)

data FeedElement = RssElement RssItem | AtomElement AtomEntry
  deriving(Eq, Show)


-- * Generic getters

getFeedTitle :: Feed -> Text
getFeedTitle (Rss doc)   = channelTitle doc
getFeedTitle (Atom feed) = show $ prettyAtomText $ feedTitle feed

getElements :: Feed -> [FeedElement]
getElements (Rss doc)   = map RssElement $ channelItems doc
getElements (Atom feed) = map AtomElement $ feedEntries feed

getDate :: FeedElement -> Maybe UTCTime
getDate (RssElement item)   = itemPubDate item
getDate (AtomElement entry) = Just $ entryUpdated entry

getTitle :: FeedElement -> Text
getTitle (RssElement item)   = itemTitle item
getTitle (AtomElement entry) = show $ prettyAtomText $ entryTitle entry

getHashes :: FeedElement -> [Int]
getHashes (RssElement item) = map (hash . (show :: Doc -> String) . prettyGuid) (maybeToList $ itemGuid item)
  <> map ((hash :: String -> Int) . show . withRssURI prettyURI) (maybeToList $ itemLink item)
  <> [hash $ itemTitle item]
  <> [hash $ itemDescription item]
getHashes (AtomElement entry) = [hash $ entryId entry, (hash :: String -> Int) $ show $ prettyAtomText $ entryTitle entry]
