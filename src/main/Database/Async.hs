{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Database.Async (withAsyncHandle) where

import           Database.Handle
import           Output                        (putDocLn)
import qualified Output

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TMChan
import           Control.Exception.Safe
import           Imm.Logger                    (LogLevel (..), log)
import qualified Imm.Logger                    as Logger
import           Imm.Pretty


-- | Run an existing database handle in a separate thread
withAsyncHandle :: m ~ IO => Logger.Handle m -> Output.Handle m -> Handle m -> (Handle m -> m ()) -> m ()
withAsyncHandle logger output database f = do
  channel <- newTMChanIO

  thread <- async $ fix $ \recurse -> do
    maybeMessage <- atomically $ readTMChan channel
    forM_ maybeMessage $ \message -> do
      interpret database message
      recurse

  log logger Debug "Database thread started"
  catchAny (f $ command channel) $ \e -> do
    log logger Error $ pretty $ displayException e
    putDocLn output $ pretty $ displayException e
  log logger Debug "Closing database thread..."
  atomically (closeTMChan channel) >> wait thread

withMVar :: MonadIO m => TMChan a -> ((b -> m ()) -> a) -> m b
withMVar channel message = do
  result <- newEmptyMVar
  atomically $ writeTMChan channel (message $ putMVar result)
  takeMVar result

withMVarE :: (MonadIO m, MonadThrow m, Exception e)
  => TMChan a -> ((Either e b -> m ()) -> a) -> m b
withMVarE channel message = do
  result <- newEmptyMVar
  atomically $ writeTMChan channel (message $ putMVar result)
  takeMVar result >>= either throwM return

asyncCommand :: MonadIO m => TMChan a -> (m () -> a) -> m ()
asyncCommand channel message = do
  atomically $ writeTMChan channel (message $ return ())

command :: MonadIO m => MonadThrow m => TMChan (HandleF (m ())) -> Handle m
command channel = Handle
  { _describeDatabase = withMVar channel DescribeDatabase
  , _fetchAllFeeds = withMVar channel FetchAllFeeds
  , _fetchFeed = withMVarE channel . FetchFeed
  , _fetchAllItems = withMVar channel FetchAllItems
  , _fetchItems = withMVar channel . FetchItems
  , _fetchItem = withMVarE channel . FetchItem
  , _updateFeedDefinition = asyncCommand channel . UpdateFeedDefinition
  , _updateFeedStatus = asyncCommand channel . UpdateFeedStatus
  , _updateItemStatus = asyncCommand channel . UpdateItemStatus
  , _insertFeed = withMVarE channel . InsertFeed
  , _insertItem = withMVar channel . InsertItem
  , _deleteFeed = asyncCommand channel . DeleteFeed
  , _purge = asyncCommand channel Purge
  , _commit = asyncCommand channel Commit
  }
