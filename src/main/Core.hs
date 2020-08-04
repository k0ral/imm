{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Core (
  markAsUnprocessed,
  subscribe,
  unsubscribe,
  listFeeds,
  describeFeed,
) where

-- {{{ Imports
import           Output                 (putDocLn)
import qualified Output

import           Control.Exception.Safe
import qualified Data.Map               as Map
import qualified Imm.Database.Feed      as Database
import           Imm.Feed
import           Imm.Logger             as Logger
import           Imm.Pretty
import           Text.XML               as XML ()
-- }}}


markAsUnprocessed :: MonadThrow m => MonadIO m
                  => Logger.Handle m
                  -> Database.Handle m
                  -> FeedQuery
                  -> m ()
markAsUnprocessed logger database query = Database.resolveEntryKey database query
  >>= mapM_ (Database.markAsUnprocessed logger database)

-- | Print database status for given feed(s)
describeFeed :: MonadThrow m => MonadIO m
             => Output.Handle m -> Database.Handle m
             -> FeedQuery -> m ()
describeFeed output database feedQuery = do
  entries <- Database.fetchQuery database (Database.matching feedQuery)
  forM_ (Map.toList entries) $ \(index, entry) ->
    putDocLn output $ pretty index <+> Database.prettyEntry entry

-- | Register the given set of feeds in database
subscribe :: MonadCatch m => MonadIO m
          => Logger.Handle m -> Output.Handle m -> Database.Handle m
          -> FeedLocation -> Set Text -> m ()
subscribe logger output database feedLocation tags = do
  index <- Database.register logger database feedLocation tags
  putDocLn output $ "Subscribed with index" <+> pretty index

-- | Un-register the given set of feeds from database
unsubscribe :: MonadThrow m
            => Logger.Handle m
            -> Database.Handle m
            -> FeedQuery
            -> m ()
unsubscribe logger database query = Database.resolveEntryKey database query
  >>= Database.delete logger database

-- | List all subscribed feeds and their status
listFeeds :: MonadIO m => MonadCatch m
          => Logger.Handle m -> Output.Handle m -> Database.Handle m
          -> m ()
listFeeds logger output database = do
  entries <- Database.fetchAll database
  when (null entries) $ log logger Warning "No subscription"
  forM_ (zip [0..] $ Map.elems entries) $ \(i, entry) -> do
    putDocLn output $ pretty (i :: Int) <+> Database.prettyShortEntry entry
