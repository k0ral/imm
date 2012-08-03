{-# LANGUAGE FlexibleContexts #-}
module Imm.Main where

-- {{{ Imports
import Imm.Config
import Imm.Feed
import qualified Imm.Mail as Mail
import qualified Imm.Maildir as Maildir
import Imm.HTTP as HTTP
import Imm.Types
import Imm.Util

import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_)
import Control.Monad.Reader hiding(forM_, mapM_)

import Data.ByteString as B (concat)
import Data.ByteString.Lazy as B hiding(putStrLn, concat)
import Data.Text.Lazy as T hiding(unlines)
import Data.Foldable
--import Data.Functor
import Data.Text.ICU.Convert
import Data.Time

import Network.URI hiding(parseURI)

import Prelude hiding(mapM_)

import System.Directory

--import Text.Feed.Import
import Text.Feed.Query hiding(getItemDate)
import Text.Feed.Types
-- }}}

-- | Internal entry point for imm, after boot process
main :: (MonadIO m, MonadError ImmError m) => FeedList -> m ()
main feeds = forM_ feeds $ \(f, u) -> (runReaderT (processFeed u) (f defaultSettings)) `catchError` (io . print)

checkStateDirectory :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => m ()
checkStateDirectory = asks mStateDirectory >>= resolve >>= try . io . createDirectoryIfMissing True


-- | Initialize maildir, download and parse feed, create mails for each new item
processFeed :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => String -> m ()
processFeed uriString = do
    logNormal $ "Processing feed: " ++ uriString
    checkStateDirectory
    Maildir.init =<< asks mMaildir
    (uri, feed) <- downloadFeed =<< parseURI uriString

    logVerbose $ unlines [
        "Title:  " ++ (getFeedTitle feed),
        "Author: " ++ (maybe "No author" id $ getFeedAuthor feed),
        "Home:   " ++ (maybe "No home"   id $ getFeedHome feed)]
    
    lastCheck <- getLastCheck uri
    forM_ (feedItems feed) $ \item -> 
      do
        date <- getItemDate item
        when (date > lastCheck) $ processItem (item, feed)
      `catchError` (io . print)

    markAsRead uri
    
      
processItem :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => (Item, Feed) -> m ()
processItem (item, feed) = do
    date <- getItemDate item
    logVerbose $ unlines [
            "   Item author: " ++ (maybe "<empty>" id $ getItemAuthor item),
            "   Item title:  " ++ (maybe "<empty>" id $ getItemTitle item),
            "   Item URI:    " ++ (maybe "<empty>" id $ getItemLink  item),
            -- "   Item Body:   " ++ (Imm.Mail.getItemContent  item),
            "   Item date:   " ++ show date]
    
    timeZone <- io getCurrentTimeZone
    dir      <- asks mMaildir
    Maildir.add dir =<< Mail.build timeZone (item, feed)


downloadFeed :: (MonadIO m, MonadError ImmError m) => URI -> m ImmFeed
downloadFeed uri = do
    feed <- parseFeedString . T.unpack =<< decode =<< HTTP.getRaw uri
    return (uri, feed)


decode :: (MonadIO m, MonadError ImmError m) => ByteString -> m Text
decode raw = do
    catchError (decodeUtf8 raw) $ return $ do
        conv <- io $ open "ISO-8859-1" Nothing
        return . T.fromChunks . (\a -> [a]) . toUnicode conv . B.concat . B.toChunks $ raw
