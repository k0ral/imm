{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Core (
  putDocLn,
  markAsUnprocessed,
  subscribe,
  unsubscribe,
  listFeeds,
  describeFeed,
  importOPML,
) where

-- {{{ Imports
import           Control.Exception.Safe
import           Data.Conduit
import qualified Data.Map                                  as Map
import qualified Data.Set                                  as Set
import qualified Data.Text.Lazy.IO                         as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Tree
import qualified Imm.Database.Feed                         as Database
import           Imm.Feed
import           Imm.Logger                                as Logger
import           Imm.Pretty
import           Refined
import           Text.OPML.Conduit.Parse
import           Text.OPML.Types                           as OPML
import           Text.XML                                  as XML ()
import           Text.XML.Stream.Parse                     as XML (def, force, parseBytes)
-- }}}


putDocLn :: MonadIO m => Doc AnsiStyle -> m ()
putDocLn = io . Text.putStrLn . renderLazy . layoutPretty defaultLayoutOptions

markAsUnprocessed :: MonadThrow m => MonadIO m
                  => Logger.Handle m
                  -> Database.Handle m
                  -> FeedQuery
                  -> m ()
markAsUnprocessed logger database query = Database.resolveEntryKey database query
  >>= mapM_ (Database.markAsUnprocessed logger database)

-- | Print database status for given feed(s)
describeFeed :: MonadThrow m => MonadIO m => Logger.Handle m -> Database.Handle m -> FeedQuery -> m ()
describeFeed logger database feedQuery = do
  entries <- Database.fetchQuery database (Database.matching feedQuery)
  forM_ (Map.toList entries) $ \(index, entry) ->
    putDocLn $ pretty index <+> Database.prettyEntry entry

-- | Register the given set of feeds in database
subscribe :: MonadCatch m => MonadIO m => Logger.Handle m -> Database.Handle m -> FeedLocation -> Set Text -> m ()
subscribe logger database feedLocation tags = do
  index <- Database.register logger database feedLocation tags
  putDocLn $ "Subscribed with index" <+> pretty index

-- | Un-register the given set of feeds from database
unsubscribe :: MonadThrow m
            => Logger.Handle m
            -> Database.Handle m
            -> FeedQuery
            -> m ()
unsubscribe logger database query = Database.resolveEntryKey database query
  >>= Database.delete logger database

-- | List all subscribed feeds and their status
listFeeds :: MonadIO m => MonadCatch m => Logger.Handle m -> Database.Handle m -> m ()
listFeeds logger database = do
  entries <- Database.fetchAll database
  when (null entries) $ log logger Warning "No subscription"
  forM_ (zip [0..] $ Map.elems entries) $ \(i, entry) -> do
    putDocLn $ pretty (i :: Int) <+> Database.prettyShortEntry entry


-- | 'subscribe' to all feeds described by the OPML document provided in input
importOPML :: MonadCatch m => MonadIO m => Logger.Handle m -> Database.Handle m -> ConduitT () ByteString m () -> m ()
importOPML logger database input = do
  opml <- runConduit $ input .| XML.parseBytes def .| force "Invalid OPML" parseOpml
  forM_ (opmlOutlines opml) $ importOPML' logger database mempty

importOPML' :: MonadCatch m => MonadIO m => Logger.Handle m -> Database.Handle m -> Set Text -> Tree OpmlOutline -> m ()
importOPML' logger database _ (Node (OpmlOutlineGeneric b _) sub) = mapM_ (importOPML' logger database (Set.singleton . unrefine $ OPML.text b)) sub
importOPML' logger database c (Node (OpmlOutlineSubscription _ s) _) = void $ subscribe logger database (FeedDirectURI $ xmlUri s) c
importOPML' _ _ _ _ = return ()
