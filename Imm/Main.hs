{-# LANGUAGE FlexibleContexts #-}
-- | Main high level process functions.
module Imm.Main where

-- {{{ Imports
import Imm.Config
import Imm.Feed as Feed
import Imm.OPML as OPML
import Imm.Types
import Imm.Util

import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_)
import Control.Monad.Reader hiding(forM_, mapM_)

import Data.Default
import Data.Foldable
import Data.Functor

import Prelude hiding(mapM_)

import System.Directory
import System.IO
-- }}}


check :: (MonadIO m) => FeedList -> m ()
check feeds = io . forM_ feeds $ \(f, u) -> do    
    result <- runErrorT . (`runReaderT` (f def)) $ do
        logNormal $ "Checking: " ++ u
        Feed.check =<< Feed.download =<< parseURI u
    either print return result


importOPML :: (MonadIO m) => m ()
importOPML = io $ (maybe (return ()) addFeeds) =<< OPML.read <$> hGetContents stdin


list :: (MonadIO m) => FeedList -> m ()
list = io . mapM_ (\(f, u) -> runReaderT (printStatus u) (f def))


markAsRead :: (MonadIO m) => FeedList -> m ()
markAsRead = mapM_ (\(f,u) -> runReaderT (runErrorT $ checkStateDirectory >> parseURI u >>= Feed.markAsRead) (f def))


markAsUnread :: (MonadIO m) => FeedList -> m ()
markAsUnread = mapM_ (\(f,u) -> runReaderT (runErrorT $ parseURI u >>= Feed.markAsUnread) (f def))


update :: (MonadIO m) => FeedList -> m ()
update feeds = io . forM_ feeds $ \(f, u) -> do
    result <- runErrorT . (`runReaderT` (f def)) $ do
        logNormal $ "Updating: " ++ u
        checkStateDirectory
        Feed.update =<< Feed.download =<< parseURI u
    either print return result


checkStateDirectory :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => m ()
checkStateDirectory = try . io . (createDirectoryIfMissing True =<<) =<< asks mStateDirectory
