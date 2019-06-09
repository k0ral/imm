{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
module Core (
-- * Types
  FeedRef,
-- * Actions
  subscribe,
  listFeeds,
  showFeed,
  importOPML,
) where

-- {{{ Imports
import qualified Imm.Database            as Database
import           Imm.Database.FeedTable
import qualified Imm.Database.FeedTable  as Database
import           Imm.Feed
import           Imm.Logger              as Logger
import           Imm.Pretty

import           Control.Exception.Safe
import           Data.Conduit
import qualified Data.Map                as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Tree
import           Refined
import           Text.OPML.Conduit.Parse
import           Text.OPML.Types         as OPML
import           Text.XML                as XML ()
import           Text.XML.Stream.Parse   as XML
import           URI.ByteString
-- }}}



-- | Print database status for given feed(s)
showFeed :: MonadThrow m => Logger.Handle m -> Database.Handle m FeedTable -> FeedID -> m ()
showFeed logger database feedID = do
  entry <- Database.fetch database feedID
  flushLogs logger
  log logger Info $ prettyDatabaseEntry entry

-- | Register the given feed URI in database
subscribe :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> URI -> Set Text -> m ()
subscribe logger database uri = Database.register logger database (FeedID uri)

-- | List all subscribed feeds and their status
listFeeds :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> m ()
listFeeds logger database = do
  entries <- Database.fetchAll database
  flushLogs logger
  when (null entries) $ log logger Warning "No subscription"
  forM_ (zip [1..] $ Map.elems entries) $ \(i, entry) ->
    log logger Info $ pretty (i :: Int) <+> prettyShortDatabaseEntry entry


-- | 'subscribe' to all feeds described by the OPML document provided in input
importOPML :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> ConduitT () ByteString m () -> m ()
importOPML logger database input = do
  opml <- runConduit $ input .| XML.parseBytes def .| force "Invalid OPML" parseOpml
  forM_ (opmlOutlines opml) $ importOPML' logger database mempty

importOPML' :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> Set Text -> Tree OpmlOutline -> m ()
importOPML' logger database _ (Node (OpmlOutlineGeneric b _) sub) = mapM_ (importOPML' logger database (Set.singleton . unrefine $ OPML.text b)) sub
importOPML' logger database c (Node (OpmlOutlineSubscription _ s) _) = subscribe logger database (xmlUri s) c
importOPML' _ _ _ _ = return ()
