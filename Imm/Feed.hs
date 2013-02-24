module Imm.Feed where

-- {{{ Imports
import Imm.Config
import Imm.Database
import Imm.Error
import qualified Imm.HTTP as HTTP
import Imm.Options hiding(markAsRead)
import Imm.Util

-- import Control.Applicative
import Control.Conditional hiding(when)
import Control.Monad.Base
import Control.Monad.Error

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Either
import Data.Functor
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Data.Text.ICU.Convert
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
-- }}}

type FeedID    = URI
type ImmFeed   = (FeedID, Feed)

describeType :: Feed -> String
describeType (AtomFeed _) = "Atom"
describeType (RSSFeed _)  = "RSS 2.x"
describeType (RSS1Feed _) = "RSS 1.x"
describeType (XMLFeed _)  = "XML"

describe :: Feed -> String
describe feed = unlines [
    "Type:   " ++ describeType feed,
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
download :: (MonadBase IO m, OptionsReader m, ConfigReader m, MonadError ImmError m) => URI -> m ImmFeed
download uri = do
    logV $ "Downloading " ++ show uri
    d <- getDecoder
    feed <- parse . TL.unpack . decodeWith d =<< HTTP.getRaw uri
    return (uri, feed)
  where
    decodeWith d = TL.fromChunks . (: []) . toUnicode d . B.concat . BL.toChunks

-- |
check :: (ConfigReader m, OptionsReader m, MonadBase IO m, MonadError ImmError m) => ImmFeed -> m ()
check (uri, feed) = do
    lastCheck       <- getLastCheck uri
    (errors, dates) <- partitionEithers <$> forM (feedItems feed) getDate
    logE . unlines $ map show errors
    let newItems = filter (> lastCheck) dates
    io . putStrLn $ "==> " ++ show (length newItems) ++ " new item(s) "


-- | Simply set the last check time to now.
markAsRead :: forall (m :: * -> *) . (MonadBase IO m, MonadError ImmError m, OptionsReader  m) => URI -> m ()
markAsRead uri = io getCurrentTime >>= storeLastCheck uri >> (logV $ "Feed " ++ show uri ++ " marked as read.")

-- | Simply remove the state file.
markAsUnread :: forall (m :: * -> *) . (MonadBase IO m, MonadError ImmError m, OptionsReader  m) => URI -> m ()
markAsUnread uri = do
    forget uri
    logV $ "Feed " ++ show uri ++ " marked as unread."


-- | Return a 'String' describing the last update for a given feed.
showStatus :: (OptionsReader m, MonadBase IO m) => URI -> m String
showStatus uri = let nullTime = posixSecondsToUTCTime 0 in do
    lastCheck <- getLastCheck uri
    return $ ((lastCheck == nullTime) ? "[NEW] " ?? ("[Last update: "++ show lastCheck ++ "]")) ++ " " ++ show uri


-- {{{ Item utilities
-- | This function is missing from 'Text.Feed.Query', probably because it is difficult to define where the content is located in a generic way for Atom/RSS 1.x/RSS 2.x feeds.
getItemContent :: Item -> String
getItemContent (AtomItem i) = length theContent < length theSummary ? theSummary ?? theContent
  where
    theContent = maybe "" extractHtml $ Atom.entryContent i
    theSummary = maybe "No content" Atom.txtToString $ Atom.entrySummary i
getItemContent (RSSItem  i) = length theContent < length theDescription ? theDescription ?? theContent
  where
    theContent     = dropWhile isSpace . concat . map concat . map (map cdData . onlyText) . map elContent . RSS.rssItemOther $ i
    theDescription = fromMaybe "No description." $ RSS.rssItemDescription i
getItemContent (RSS1Item i) = concat . catMaybes . map (RSS1.contentValue) . RSS1.itemContent $ i
getItemContent item         = fromMaybe "No content." . getItemDescription $ item


getDate :: (ConfigReader m, Monad m) => Item -> m (Either ImmError UTCTime)
getDate x = do
    parsers <- readConfig dateParsers
    return $ maybe (Left $ ParseItemDateError x) Right $ parseDateWith parsers =<< F.getItemDate x

parseDateWith :: [String -> Maybe UTCTime] -> String -> Maybe UTCTime
parseDateWith parsers date = listToMaybe . {-map T.zonedTimeToUTC .-} catMaybes . flip map parsers $ \f -> f . TL.unpack . TL.strip . TL.pack $ date
-- }}}


extractHtml :: Atom.EntryContent -> String
extractHtml (Atom.HTMLContent c) = c
extractHtml (Atom.XHTMLContent c) = strContent c
extractHtml (Atom.TextContent t) = t
extractHtml (Atom.MixedContent a b) = show a ++ show b
extractHtml (Atom.ExternalContent mediaType uri) = show mediaType ++ show uri
