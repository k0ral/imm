{-# LANGUAGE TupleSections #-}
module Imm.Feed where

-- {{{ Imports
import Imm.Database
import Imm.Error
import qualified Imm.HTTP as HTTP
import Imm.Util hiding(when)

import Control.Monad.Error

-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Data.Time as T hiding(parseTime)
import Data.Time.Clock.POSIX

import Network.URI as N

import qualified Text.Atom.Feed as Atom
import qualified Text.RSS1.Syntax as RSS1
import qualified Text.RSS.Syntax as RSS
import Text.Feed.Import as F
import Text.Feed.Query as F
import Text.Feed.Types as F
import Text.XML.Light.Proc
import Text.XML.Light.Types

import System.Log.Logger
-- }}}

-- {{{ Types
data Action = Check | ShowStatus | MarkAsRead | MarkAsUnread | Update
    deriving(Eq, Show)

type ImmFeed = (FeedID, Feed)

class FeedParser m where
    parseDate :: String -> m (Maybe UTCTime)

instance (Monad m, Error e, FeedParser m) => FeedParser (ErrorT e m) where
    parseDate = lift . parseDate
-- }}}


-- | Provide a 'String' representation of the feed type.
showType :: Feed -> String
showType (AtomFeed _) = "Atom"
showType (RSSFeed _)  = "RSS 2.x"
showType (RSS1Feed _) = "RSS 1.x"
showType (XMLFeed _)  = "XML"

describe :: Feed -> String
describe feed = unlines [
    "Type:   " ++ showType feed,
    "Title:  " ++ getFeedTitle feed,
    "Author: " ++ fromMaybe "No author" (getFeedAuthor feed),
    "Home:   " ++ fromMaybe "No home"   (getFeedHome feed)]

describeItem :: Item -> String
describeItem item = unlines [
    "   Item author: " ++ fromMaybe "<empty>" (getItemAuthor item),
    "   Item title:  " ++ fromMaybe "<empty>" (getItemTitle item),
    "   Item URI:    " ++ fromMaybe "<empty>" (getItemLink  item),
    -- "   Item Body:   " ++ (Imm.Mail.getItemContent  item),
    "   Item date:   " ++ fromMaybe "<empty>" (getItemDate item)]

-- | Monad-agnostic version of 'Text.Feed.Import.parseFeedString'
parse :: MonadError ImmError m => String -> m Feed
parse x = maybe (throwError $ ParseFeedError x) return $ parseFeedString x


-- | Retrieve, decode and parse the given resource as a feed.
download :: (HTTP.Decoder m, MonadBase IO m, MonadError ImmError m) => URI -> m ImmFeed
download uri = do
    io . debugM "imm.feed" $ "Downloading " ++ show uri
    fmap (uri,) . parse . TL.unpack =<< HTTP.get uri

-- | Count the list of unread items for given feed.
check :: (FeedParser m, DatabaseReader m, MonadBase IO m, MonadError ImmError m) => ImmFeed -> m ()
check (feedID, feed) = do
    lastCheck       <- getLastCheck feedID
    (errors, dates) <- tryGetDates $ feedItems feed
    let newItems     = filter (> lastCheck) dates

    unless (null errors) . io . errorM "imm.feed" . unlines $ map show errors
    io . noticeM "imm.feed" $ show (length newItems) ++ " new item(s) for <" ++ show feedID ++ ">"
  where
    tryGetDates = fmap partitionEithers . mapM (runErrorT . getDate)

-- | Simply set the last check time to now.
markAsRead :: (MonadBase IO m, MonadError ImmError m, DatabaseWriter m) => URI -> m ()
markAsRead uri = do
    io getCurrentTime >>= storeLastCheck uri
    io . noticeM "imm.feed" $ "Feed <" ++ show uri ++ "> marked as read."

-- | Simply remove the state file.
markAsUnread ::  (MonadBase IO m, MonadError ImmError m, DatabaseWriter m) => URI -> m ()
markAsUnread uri = do
    forget uri
    io . noticeM "imm.feed" $ "Feed <" ++ show uri ++ "> marked as unread."

-- | Return a 'String' describing the last update for a given feed.
showStatus :: (DatabaseReader m, MonadBase IO m) => URI -> m String
showStatus uri = let nullTime = posixSecondsToUTCTime 0 in do
    lastCheck <- getLastCheck uri
    return $ ((lastCheck == nullTime) ? "[NEW] " ?? ("[Last update: "++ show lastCheck ++ "]")) ++ " " ++ show uri


-- {{{ Item utilities
-- | This function is missing from 'Text.Feed.Query', probably because it is difficult to define where the content is located in a generic way for Atom/RSS 1.x/RSS 2.x feeds.
getItemContent :: Item -> String
getItemContent (AtomItem i) = length theContent < length theSummary ? theSummary ?? theContent
  where
    theContent = fromMaybe "" (extractHtml <$> Atom.entryContent i)
    theSummary = fromMaybe "No content" (Atom.txtToString <$> Atom.entrySummary i)
getItemContent (RSSItem  i) = length theContent < length theDescription ? theDescription ?? theContent
  where
    theContent     = dropWhile isSpace . concatMap concat . map (map cdData . onlyText . elContent) . RSS.rssItemOther $ i
    theDescription = fromMaybe "No description." $ RSS.rssItemDescription i
getItemContent (RSS1Item i) = concat . mapMaybe RSS1.contentValue . RSS1.itemContent $ i
getItemContent item         = fromMaybe "No content." . getItemDescription $ item

getDate :: (FeedParser m, Monad m, MonadError ImmError m) => Item -> m UTCTime
getDate item = maybe (throwError $ ParseItemDateError item) return =<< maybe (return Nothing) parseDate =<< return (getItemDate item)
-- }}}


extractHtml :: Atom.EntryContent -> String
extractHtml (Atom.HTMLContent c) = c
extractHtml (Atom.XHTMLContent c) = strContent c
extractHtml (Atom.TextContent t) = t
extractHtml (Atom.MixedContent a b) = show a ++ show b
extractHtml (Atom.ExternalContent mediaType uri) = show mediaType ++ show uri
