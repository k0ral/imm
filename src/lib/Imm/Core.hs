{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module Imm.Core (
-- * Types
  FeedRef,
-- * Actions
  printVersions,
  subscribe,
  showFeed,
  check,
  run,
  importOPML,
) where

-- {{{ Imports
import qualified Imm.Database                as Database
import           Imm.Database.FeedTable
import qualified Imm.Database.FeedTable      as Database
import           Imm.Feed
import           Imm.Hooks                   as Hooks
import qualified Imm.HTTP                    as HTTP
import           Imm.Logger as Logger
import           Imm.Prelude
import           Imm.Pretty
import           Imm.XML as XML

import           Control.Concurrent.STM      (STM, atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Time
import           Data.Conduit
import qualified Data.Map                    as Map
import           Data.NonNull
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import           Data.Tree
import           Data.Version
import qualified Paths_imm                   as Package
import           Streamly                    hiding ((<>))
import qualified Streamly.Prelude            as Stream
import           System.Info
import           Text.OPML.Conduit.Parse
import           Text.OPML.Types             as OPML
import           Text.XML                    as XML ()
import           Text.XML.Stream.Parse       as XML
import           URI.ByteString
-- }}}


printVersions :: (MonadBase IO m) => m ()
printVersions = liftBase $ do
  putStrLn $ "imm-" <> Text.pack (showVersion Package.version)
  putStrLn $ "compiled by " <> Text.pack compilerName <> "-" <> Text.pack (showVersion compilerVersion)

-- | Print database status for given feed(s)
showFeed :: MonadThrow m => Logger.Handle m -> Database.Handle m FeedTable -> [FeedID] -> m ()
showFeed logger database feedIDs = do
  entries <- Database.fetchList database feedIDs
  flushLogs logger
  when (null entries) $ log logger Warning "No subscription"
  forM_ (zip [1..] $ Map.elems entries) $ \(i, entry) ->
    log logger Info $ pretty (i :: Int) <+> prettyDatabaseEntry entry

-- | Register the given feed URI in database
subscribe :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> URI -> Set Text -> m ()
subscribe logger database uri = Database.register logger database (FeedID uri)

-- | Check for unread elements without processing them
check :: (MonadAsync m, MonadCatch m)
      => Logger.Handle m -> Database.Handle m FeedTable -> HTTP.Handle m -> XML.Handle m -> [FeedID] -> m ()
check logger database httpClient xmlParser feedIDs = do
  progress <- liftBase $ newTVarIO 0

  results <- Stream.toList $ wAsyncly $ do
    feedID <- Stream.fromFoldable feedIDs
    result <- lift $ tryAny $ checkOne logger database httpClient xmlParser feedID
    let logResult = either (red . pretty . displayException) (\n -> green (pretty n) <+> "new element(s)") result
    n <- liftBase $ atomically $ do
      modifyTVar (progress :: TVar Int) (+ 1)
      readTVar progress
    lift $ log logger Info $ brackets (fill width (bold $ cyan $ pretty n) <+> "/" <+> pretty total) <+> "Checked" <+> magenta (pretty feedID) <+> "=>" <+> logResult
    return result

  flushLogs logger

  let (failures, successes) = partitionEithers $ zipWith (\a -> bimap (a,) (a,)) feedIDs results
  unless (null failures) $ log logger Error $ bold (pretty $ length failures) <+> "feeds in error"
  log logger Info $ bold (pretty $ sum $ map snd successes) <+> "new element(s) overall"

  where width = length (show total :: String)
        total = length feedIDs

checkOne :: (MonadBase IO m, MonadCatch m)
         => Logger.Handle m -> Database.Handle m FeedTable -> HTTP.Handle m -> XML.Handle m -> FeedID -> m Int
checkOne logger database httpClient xmlParser feedID = do
  feed <- getFeed logger httpClient xmlParser feedID
  case feed of
    Atom _ -> log logger Debug $ "Parsed Atom feed: " <> pretty feedID
    Rss _  -> log logger Debug $ "Parsed RSS feed: " <> pretty feedID

  let dates = mapMaybe getDate $ getElements feed

  log logger Debug $ vsep $ map prettyElement $ getElements feed
  status <- Database.getStatus database feedID

  return $ length $ filter (unread status) dates
  where unread (LastUpdate t1) t2 = t2 > t1
        unread _ _                = True


run :: (MonadTime m, MonadAsync m, MonadCatch m)
    => Logger.Handle m -> Database.Handle m FeedTable -> HTTP.Handle m -> Hooks.Handle m -> XML.Handle m -> [FeedID] -> m ()
run logger database httpClient hooks xmlParser feedIDs = do
  progress <- liftBase $ newTVarIO 0

  results <- Stream.toList $ wAsyncly $ do
    feedID <- Stream.fromFoldable feedIDs
    result <- lift $ tryAny $ runOne logger database httpClient hooks xmlParser feedID
    let logResult = either (red . pretty . displayException) (\n -> green (pretty n) <+> "new element(s)") result
    n <- liftBase $ atomically $ do
      modifyTVar progress (+ 1)
      readTVar progress :: STM Int
    lift $ log logger Info $ brackets (fill width (bold $ cyan $ pretty n) <+> "/" <+> pretty total) <+> "Processed" <+> magenta (pretty feedID) <+> "=>" <+> logResult
    return $ bimap (feedID,) (feedID,) result

  flushLogs logger

  let (failures, successes) = partitionEithers results

  unless (null failures) $ log logger Error $ bold (pretty $ length failures) <+> "feeds in error"
  log logger Info $ bold (pretty $ sum $ map snd successes) <+> "new element(s) overall"

  where width = length (show total :: String)
        total = length feedIDs

runOne :: (MonadTime m, MonadCatch m)
       => Logger.Handle m -> Database.Handle m FeedTable -> HTTP.Handle m -> Hooks.Handle m -> XML.Handle m -> FeedID -> m Int
runOne logger database httpClient hooks xmlParser feedID = do
  feed <- getFeed logger httpClient xmlParser feedID
  unreadElements <- filterM (fmap not . isRead database feedID) $ getElements feed

  forM_ unreadElements $ \element -> do
    onNewElement logger hooks feed element
    mapM_ (Database.addReadHash logger database feedID) $ getHashes element

  Database.markAsRead logger database feedID
  return $ length unreadElements


isRead :: MonadCatch m => Database.Handle m FeedTable -> FeedID -> FeedElement -> m Bool
isRead database feedID element = do
  DatabaseEntry _ _ readHashes lastCheck <- Database.fetch database feedID
  let matchHash = not $ null $ (setFromList (getHashes element) :: Set Int) `intersection` readHashes
      matchDate = case (lastCheck, getDate element) of
        (Nothing, _)     -> False
        (_, Nothing)     -> False
        (Just a, Just b) -> a > b
  return $ matchHash || matchDate

-- | 'subscribe' to all feeds described by the OPML document provided in input
importOPML :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> ConduitT () ByteString m () -> m ()
importOPML logger database input = do
  opml <- runConduit $ input .| XML.parseBytes def .| force "Invalid OPML" parseOpml
  forM_ (opmlOutlines opml) $ importOPML' logger database mempty

importOPML' :: MonadCatch m => Logger.Handle m -> Database.Handle m FeedTable -> Set Text -> Tree OpmlOutline -> m ()
importOPML' logger database _ (Node (OpmlOutlineGeneric b _) sub) = mapM_ (importOPML' logger database (Set.singleton . toNullable $ OPML.text b)) sub
importOPML' logger database c (Node (OpmlOutlineSubscription _ s) _) = subscribe logger database (xmlUri s) c
importOPML' _ _ _ _ = return ()


getFeed :: MonadCatch m => Logger.Handle m -> HTTP.Handle m -> XML.Handle m -> FeedID -> m Feed
getFeed logger httpClient xmlParser (FeedID uri) = HTTP.get logger httpClient uri >>= parseXml xmlParser uri
