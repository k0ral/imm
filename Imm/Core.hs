module Imm.Core where

-- {{{ Imports
import Imm.Feed
import Imm.Mail
import qualified Imm.Maildir as Maildir
import Imm.Types
import Imm.Util

import Control.Error
import Control.Monad hiding(forM_, mapM_)

import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.UTF8 as B
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding
import Data.Foldable
--import Data.Functor
import Data.Time

import Network.HTTP hiding(Response)
import Network.Stream
import Network.URI

import Prelude hiding(mapM_)

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- Entry point
realMain :: [FeedGroup] -> (Settings, CliOptions) -> IO ()
realMain feedGroups (settings, _options) = do
    result <- forM feedGroups $ runEitherT . processFeedGroup settings
    forM_ (lefts result) print

   
processFeedGroup :: Settings -> FeedGroup -> EitherT ImmError IO ()
processFeedGroup globalSettings _feedGroup@(feedSettings, feedURIs) = do
    fmapLT OtherError $ Maildir.init $ mMailDirectory feedSettings
    forM_ feedURIs $ \uri -> do
        uri' <- EitherT . return $ parseURI' uri
        feed <- downloadFeed uri'
        processFeed globalSettings feedSettings feed
  where
    parseURI' uri = note (ParseUriError uri) . parseURI $ uri


processFeed :: Settings -> FeedSettings -> ImmFeed -> EitherT ImmError IO ()
processFeed globalSettings feedSettings (uri, feed) = do
    io . logVerbose $ unlines [
        "Processing feed: " ++ show uri,
        ("Title:  " ++) . getFeedTitle $ feed,
        ("Author: " ++) . maybe "No author" id . getFeedAuthor $ feed,
        ("Home:   " ++) . maybe "No home"   id . getFeedHome $ feed]
    
    lastCheck <- io $ getLastCheck globalSettings (uri, feed)
    forM_ (feedItems feed) $ \item -> do
      date <- EitherT . return $ getItemDate' item
      when (date > lastCheck) $ fmapLT OtherError $ processItem globalSettings feedSettings item
      return ()
    
    currentTime <- io getCurrentTime
    storeLastCheck globalSettings (uri, feed) currentTime
  where
    getItemDate' x = note (ParseItemDateError x) . (parseDate <=< getItemDate) $ x
      
      
processItem :: Settings -> FeedSettings -> Item -> Script ()
processItem _globalSettings feedSettings item = do
    io . logVerbose $ unlines ["",
            "   Item author: " ++ (maybe "" id $ getItemAuthor item),
            "   Item title:  " ++ (maybe "" id $ getItemTitle item),
            "   Item URI:    " ++ (maybe "" id $ getItemLink  item),
            --"   Item Content:    " ++ (Imm.Mail.getItemContent  item),
            "   Item date:   " ++ (maybe "" id $ time)]
    
    timeZone <- io getCurrentTimeZone
    void $ Maildir.add dir . itemToMail timeZone $ item 
  where
    time = getItemDate item
    dir  = mMailDirectory feedSettings


downloadRaw :: URI -> EitherT ConnError IO T.Text
downloadRaw uri = do
    io . logVerbose $ "Downloading " ++ show uri
    response <- EitherT $ simpleHTTP (mkRequest GET uri :: Request B.ByteString)
    return . decodeUtf8 . rspBody $ response


downloadFeed :: URI -> EitherT ImmError IO ImmFeed
downloadFeed uri = do
    rawData <- fmapLT CE $ downloadRaw uri
    feed    <- EitherT . return $ parseFeedString' (T.unpack rawData)
    return (uri, feed)
  where
    parseFeedString' x = note (ParseFeedError $ show x) $ parseFeedString x
