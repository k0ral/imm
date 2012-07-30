{-# LANGUAGE FlexibleContexts #-}
module Imm.Main where

-- {{{ Imports
import Imm.Feed
import qualified Imm.Mail as Mail
import qualified Imm.Maildir as Maildir
import Imm.Types
import Imm.Util

import Control.Conditional hiding(when)
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_)
import Control.Monad.Reader hiding(forM_, mapM_)

import Data.ByteString as B (concat)
import Data.ByteString.Lazy as B hiding(putStrLn, concat)
import Data.Text.Lazy as T hiding(unlines)
import Data.Either
import Data.Foldable
--import Data.Functor
import Data.Text.ICU.Convert
import Data.Time
import Data.Time.Clock.POSIX

import Network.Browser hiding(request)
import Network.HTTP
import Network.URI hiding(parseURI)

import Prelude hiding(mapM_)

import System.Directory

--import Text.Feed.Import
import Text.Feed.Query hiding(getItemDate)
import Text.Feed.Types
-- }}}

-- {{{ Quick actions performed for specific commandline options
printFeedGroupStatus :: (MonadReader Settings m, MonadIO m) => FeedGroup -> m ()
printFeedGroupStatus (settings, feeds) = do
    maildir <- resolve $ mMaildir settings
    io . putStrLn $ " => Feed group " ++ maildir
    mapM_ printFeedStatus feeds

printFeedStatus :: (MonadReader Settings m, MonadIO m) => String -> m ()
printFeedStatus feedUri = do
    prefix <- case parseURI feedUri of
        Right uri -> do
          lastCheck <- getLastCheck uri
          return $ (lastCheck == posixSecondsToUTCTime 0) ? "[NEW] " ?? ("[Last update: "++ show lastCheck ++ "]")
        _ -> return "[Not an URI]"
    io . putStrLn $ prefix ++ " " ++ feedUri

   
checkFeedGroup :: (MonadReader Settings m, MonadIO m) => FeedGroup -> m ()
checkFeedGroup (settings, feeds) = return ()
-- }}}

-- | Internal entry point for imm, after boot process
main :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => m ()
main = do
    checkStateDirectory    
    asks mFeedGroups >>= mapM_ (\x -> processFeedGroup x `catchError` (io . print))

checkStateDirectory :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => m ()
checkStateDirectory = asks mStateDirectory >>= resolve >>= try . io . createDirectoryIfMissing True


processFeedGroup :: (MonadIO m, MonadReader Settings m, MonadError ImmError m) => FeedGroup -> m ()
processFeedGroup _feedGroup@(config, feedURIs) = do
    Maildir.init $ mMaildir config
    forM_ feedURIs $ \uri ->
        (parseURI uri >>= downloadFeed >>= processFeed config)
        `catchError` (io . print)


processFeed :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => FeedSettings -> ImmFeed -> m ()
processFeed feedSettings (uri, feed) = do
    logVerbose $ unlines [
        "Processing feed: " ++ show uri,
        "Title:  " ++ (getFeedTitle feed),
        "Author: " ++ (maybe "No author" id $ getFeedAuthor feed),
        "Home:   " ++ (maybe "No home"   id $ getFeedHome feed)]
    
    lastCheck <- getLastCheck uri
    forM_ (feedItems feed) $ \item -> 
      do
        date <- getItemDate item
        when (date > lastCheck) $ processItem feedSettings (item, feed)
      `catchError` (io . print)

    storeLastCheck uri =<< io getCurrentTime
    
      
processItem :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => FeedSettings -> (Item, Feed) -> m ()
processItem feedSettings (item, feed) = do
    date <- getItemDate item
    logVerbose $ unlines [
            "   Item author: " ++ (maybe "" id $ getItemAuthor item),
            "   Item title:  " ++ (maybe "" id $ getItemTitle item),
            "   Item URI:    " ++ (maybe "" id $ getItemLink  item),
            -- "   Item Body:   " ++ (Imm.Mail.getItemContent  item),
            "   Item date:   " ++ show date]
    
    timeZone <- io getCurrentTimeZone
    Maildir.add dir =<< Mail.build timeZone (item, feed)
  where
    dir = mMaildir feedSettings


downloadRaw :: (MonadIO m, MonadError ImmError m) => URI -> m ByteString
downloadRaw uri = do
    logVerbose $ "Downloading " ++ show uri
    (_, r) <- io . browse $ do
        setAllowRedirects True
        request (mkRequest GET uri :: Request ByteString)
    when (rspCode r == (4,0,4)) $ throwError (HTTPError $ rspReason r)
    return . rspBody $ r


downloadFeed :: (MonadIO m, MonadError ImmError m) => URI -> m ImmFeed
downloadFeed uri = do
    feed <- parseFeedString . T.unpack =<< decode =<< downloadRaw uri
    return (uri, feed)

decode :: (MonadIO m, MonadError ImmError m) => ByteString -> m Text
decode raw = do
    catchError (decodeUtf8 raw) $ return $ do
        conv <- io $ open "ISO-8859-1" Nothing
        return . T.fromChunks . (\a -> [a]) . toUnicode conv . B.concat . B.toChunks $ raw
