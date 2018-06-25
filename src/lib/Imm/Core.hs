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
import           Imm.Database                (MonadDatabase)
import qualified Imm.Database                as Database
import           Imm.Database.FeedTable
import qualified Imm.Database.FeedTable      as Database
import           Imm.Feed
import           Imm.Hooks                   as Hooks
import           Imm.HTTP                    (MonadHttpClient)
import qualified Imm.HTTP                    as HTTP
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty
import           Imm.XML

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
showFeed :: (MonadLog m, MonadThrow m, MonadDatabase FeedTable m)
         => [FeedID] -> m ()
showFeed feedIDs = do
  entries <- Database.fetchList FeedTable feedIDs
  flushLogs
  when (null entries) $ logWarning "No subscription"
  forM_ (zip [1..] $ Map.elems entries) $ \(i, entry) ->
    logInfo $ pretty (i :: Int) <+> prettyDatabaseEntry entry

-- | Register the given feed URI in database
subscribe :: (MonadLog m, MonadDatabase FeedTable m, MonadCatch m)
          => URI -> Set Text -> m ()
subscribe uri = Database.register (FeedID uri)

-- | Check for unread elements without processing them
check :: (MonadAsync m, MonadCatch m, MonadLog m, MonadDatabase FeedTable m, MonadHttpClient m, MonadXmlParser m)
      => [FeedID] -> m ()
check feedIDs = do
  progress <- liftBase $ newTVarIO 0

  results <- Stream.toList $ wAsyncly $ do
    feedID <- Stream.fromFoldable feedIDs
    result <- lift $ tryAny $ checkOne feedID
    let logResult = either (red . pretty . displayException) (\n -> green (pretty n) <+> "new element(s)") result
    n <- liftBase $ atomically $ do
      modifyTVar (progress :: TVar Int) (+ 1)
      readTVar progress
    lift $ logInfo $ brackets (fill width (bold $ cyan $ pretty n) <+> "/" <+> pretty total) <+> "Checked" <+> magenta (pretty feedID) <+> "=>" <+> logResult
    return result

  flushLogs

  let (failures, successes) = partitionEithers $ zipWith (\a -> bimap (a,) (a,)) feedIDs results
  unless (null failures) $ logError $ bold (pretty $ length failures) <+> "feeds in error"
  logInfo $ bold (pretty $ sum $ map snd successes) <+> "new element(s) overall"

  where width = length (show total :: String)
        total = length feedIDs

checkOne :: (MonadBase IO m, MonadCatch m, MonadLog m, MonadDatabase FeedTable m, MonadHttpClient m, MonadXmlParser m)
         => FeedID -> m Int
checkOne feedID = do
  feed <- getFeed feedID
  case feed of
    Atom _ -> logDebug $ "Parsed Atom feed: " <> pretty feedID
    Rss _  -> logDebug $ "Parsed RSS feed: " <> pretty feedID

  let dates = mapMaybe getDate $ getElements feed

  logDebug $ vsep $ map prettyElement $ getElements feed
  status <- Database.getStatus feedID

  return $ length $ filter (unread status) dates
  where unread (LastUpdate t1) t2 = t2 > t1
        unread _ _                = True


run :: (MonadTime m, MonadAsync m, MonadCatch m, MonadImm m, MonadLog m, MonadDatabase FeedTable m, MonadHttpClient m, MonadXmlParser m)
    => [FeedID] -> m ()
run feedIDs = do
  progress <- liftBase $ newTVarIO 0

  results <- Stream.toList $ wAsyncly $ do
    feedID <- Stream.fromFoldable feedIDs
    result <- lift $ tryAny $ runOne feedID
    let logResult = either (red . pretty . displayException) (\n -> green (pretty n) <+> "new element(s)") result
    n <- liftBase $ atomically $ do
      modifyTVar progress (+ 1)
      readTVar progress :: STM Int
    lift $ logInfo $ brackets (fill width (bold $ cyan $ pretty n) <+> "/" <+> pretty total) <+> "Processed" <+> magenta (pretty feedID) <+> "=>" <+> logResult
    return $ bimap (feedID,) (feedID,) result

  flushLogs

  let (failures, successes) = partitionEithers results

  unless (null failures) $ logError $ bold (pretty $ length failures) <+> "feeds in error"
  logInfo $ bold (pretty $ sum $ map snd successes) <+> "new element(s) overall"

  where width = length (show total :: String)
        total = length feedIDs

runOne :: (MonadTime m, MonadCatch m, MonadImm m, MonadLog m, MonadDatabase FeedTable m, MonadHttpClient m, MonadXmlParser m)
       => FeedID -> m Int
runOne feedID = do
  feed <- getFeed feedID
  unreadElements <- filterM (fmap not . isRead feedID) $ getElements feed

  forM_ unreadElements $ \element -> do
    onNewElement feed element
    mapM_ (Database.addReadHash feedID) $ getHashes element

  Database.markAsRead feedID
  return $ length unreadElements


isRead :: (MonadCatch m, MonadDatabase FeedTable m) => FeedID -> FeedElement -> m Bool
isRead feedID element = do
  DatabaseEntry _ _ readHashes lastCheck <- Database.fetch FeedTable feedID
  let matchHash = not $ null $ (setFromList (getHashes element) :: Set Int) `intersection` readHashes
      matchDate = case (lastCheck, getDate element) of
        (Nothing, _)     -> False
        (_, Nothing)     -> False
        (Just a, Just b) -> a > b
  return $ matchHash || matchDate

-- | 'subscribe' to all feeds described by the OPML document provided in input
importOPML :: (MonadLog m, MonadDatabase FeedTable m, MonadCatch m)
           => ConduitT () ByteString m () -> m ()
importOPML input = do
  opml <- runConduit $ input .| XML.parseBytes def .| force "Invalid OPML" parseOpml
  forM_ (opmlOutlines opml) $ importOPML' mempty

importOPML' :: (MonadLog m, MonadDatabase FeedTable m, MonadCatch m)
            => Set Text -> Tree OpmlOutline -> m ()
importOPML' _ (Node (OpmlOutlineGeneric b _) sub) = mapM_ (importOPML' (Set.singleton . toNullable $ OPML.text b)) sub
importOPML' c (Node (OpmlOutlineSubscription _ s) _) = subscribe (xmlUri s) c
importOPML' _ _ = return ()


getFeed :: (MonadCatch m, MonadHttpClient m, MonadLog m, MonadXmlParser m)
        => FeedID -> m Feed
getFeed (FeedID uri) = HTTP.get uri >>= parseXml uri
