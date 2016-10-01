{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
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
import qualified Imm.Database.FeedTable as Database
import Imm.Database.FeedTable hiding(markAsRead, markAsUnread)
import qualified Imm.Database as Database
import Imm.Feed
import Imm.Hooks as Hooks
import Imm.HTTP (HttpClientF)
import qualified Imm.HTTP as HTTP
import Imm.Logger
import Imm.Prelude
import Imm.Pretty

-- import Control.Concurrent.Async.Lifted (Async, async, mapConcurrently, waitAny)
-- import Control.Concurrent.Async.Pool
import Control.Monad.Free

import qualified Data.ByteString              as ByteString
import Data.Conduit
import Data.Conduit.Combinators as Conduit (stdin)
import Data.Conduit.Parser
import qualified Data.Map as Map
import Data.NonNull
import Data.Set (Set)
import Data.Tree
import           Data.Version

import qualified Paths_imm                      as Package

import Rainbow hiding((<>))
import Rainbox

import           System.Info

import Text.Atom.Conduit.Parse
import Text.Atom.Types
import Text.OPML.Conduit.Parse
import Text.OPML.Types as OPML
import Text.RSS.Conduit.Parse
import Text.RSS.Types
import Text.XML as XML ()
import Text.XML.Stream.Parse as XML

import           URI.ByteString
-- }}}


printVersions :: (MonadIO m) => m ()
printVersions = io $ do
  putStrLn $ "imm-" ++ showVersion Package.version
  putStrLn $ "compiled by " ++ compilerName ++ "-" ++ showVersion compilerVersion

-- | Print database status for given feed(s)
showFeed :: (MonadIO m, LoggerF :<: f, MonadThrow m, Functor f, MonadFree f m, DatabaseF' :<: f)
         => [FeedID] -> m ()
showFeed feedIDs = do
  feeds <- Database.fetchList FeedTable feedIDs
  if null feeds then logWarning "No subscription" else putBox $ entryTableToBox feeds

-- | Register the given feed URI in database
subscribe :: (LoggerF :<: f, Functor f, MonadFree f m, DatabaseF' :<: f, MonadCatch m)
          => URI -> Maybe Text -> m ()
subscribe uri category = Database.register (FeedID uri) $ fromMaybe "default" category

-- | Check for unread elements without processing them
check :: (MonadIO m, MonadCatch m, LoggerF :<: f, Functor f, MonadFree f m, DatabaseF' :<: f, HttpClientF :<: f)
      => [FeedID] -> m ()
check feedIDs = do
  results <- forM (zip ([1..] :: [Int]) feedIDs) $ \(i, f) -> do
    logInfo $ "Checking entry " <> show i <> "/" <> show total <> "..."
    try $ checkOne f

  putBox $ statusTableToBox $ mapFromList $ zip feedIDs results
  where total = length feedIDs

checkOne :: (MonadIO m, MonadCatch m, LoggerF :<: f, Functor f, MonadFree f m, DatabaseF' :<: f, HttpClientF :<: f)
         => FeedID -> m Int
checkOne feedID@(FeedID uri) = do
  body <- HTTP.get uri
  feed <- runConduit $ parseLBS def body =$= runConduitParser ((Left <$> atomFeed) <|> (Right <$> rssDocument))

  case feed of
    Left _ -> logDebug $ "Parsed Atom feed: " <> show (pretty feedID)
    Right _ -> logDebug $ "Parsed RSS feed: " <> show (pretty feedID)

  let dates = either
              (map entryUpdated . feedEntries)
              (mapMaybe itemPubDate . channelItems)
              feed

  logDebug . show . vsep $ either (map prettyEntry . feedEntries) (map prettyItem . channelItems) feed
  status <- Database.getStatus feedID

  return $ length $ filter (unread status) dates
  where unread (LastUpdate t1) t2 = t2 > t1
        unread _ _                = True


run :: (MonadIO m, MonadCatch m, HooksF :<: f, LoggerF :<: f, Functor f, MonadFree f m, DatabaseF' :<: f, HttpClientF :<: f)
    => [FeedID] -> m ()
run feedIDs = forM_ feedIDs $ \feedID@(FeedID uri) -> do
  body <- HTTP.get uri
  feed <- runConduit $ parseLBS def body =$= runConduitParser ((Atom <$> atomFeed) <|> (Rss <$> rssDocument))
  unreadElements <- filterM (fmap not . isRead feedID) $ getElements feed

  logInfo $ show (length unreadElements) <> " unread element(s) for " <> show (pretty feedID)

  forM_ unreadElements $ \element -> do
    onNewElement feed element
    mapM_ (Database.addReadHash feedID) $ getHashes element

  Database.markAsRead feedID


isRead :: (Functor f, MonadCatch m, DatabaseF' :<: f, MonadFree f m) => FeedID -> FeedElement -> m Bool
isRead feedID element = do
  DatabaseEntry _ _ readHashes lastCheck <- Database.fetch FeedTable feedID
  let matchHash = not $ null $ (setFromList (getHashes element) :: Set Int) `intersection` readHashes
      matchDate = case (lastCheck, getDate element) of
        (Nothing, _) -> False
        (_, Nothing) -> False
        (Just a, Just b) -> a > b
  return $ matchHash || matchDate

-- | 'subscribe' to all feeds described by the OPML document provided in input (stdin)
importOPML :: (MonadIO m, LoggerF :<: f, Functor f, MonadFree f m, DatabaseF' :<: f, MonadCatch m) => m ()
importOPML = do
  opml <- runConduit $ Conduit.stdin =$= XML.parseBytes def =$= runConduitParser parseOpml
  forM_ (opmlOutlines opml) $ importOPML' mempty

importOPML' :: (MonadIO m, LoggerF :<: f, Functor f, MonadFree f m, DatabaseF' :<: f, MonadCatch m)
            => Maybe Text -> Tree OpmlOutline -> m ()
importOPML' _ (Node (OpmlOutlineGeneric b _) sub) = mapM_ (importOPML' (Just . toNullable $ OPML.text b)) sub
importOPML' c (Node (OpmlOutlineSubscription _ s) _) = subscribe (xmlUri s) c
importOPML' _ _ = return ()


-- * Boxes

putBox :: (Orientation a, MonadIO m) => Box a -> m ()
putBox = io . mapM_ ByteString.putStr . chunksToByteStrings toByteStringsColors256 . toList . render

cell :: Text -> Cell
cell a = Cell (singleton $ singleton $ chunk a) top left mempty

type EntryTable = Map FeedID DatabaseEntry

entryTableToBox :: EntryTable -> Box Horizontal
entryTableToBox t = tableByColumns $ Rainbox.intersperse sep $ fromList [col1, col2, col3, col4] where
  result = sortBy (comparing entryURI) $ map snd $ mapToList t
  col1 = fromList $ cell "UID" : map (cell . show) [0..(length result - 1)]
  col2 = fromList $ cell "CATEGORY" : map (cell . entryCategory) result
  col3 = fromList $ cell "LAST CHECK" : map (cell . format . entryLastCheck) result
  col4 = fromList $ cell "FEED URI" : map (cell . show . prettyURI . entryURI) result
  format = maybe "<never>" show
  sep = fromList [separator mempty 1]


type StatusTable = Map FeedID (Either SomeException Int)

statusTableToBox :: StatusTable -> Box Horizontal
statusTableToBox t = tableByColumns $ Rainbox.intersperse sep $ fromList [col1, col2, col3] where
  result = sortBy (comparing fst) $ Map.toList t
  col1 = fromList $ cell "# UNREAD" : map (cell . either (const "?") show . snd) result
  col2 = fromList $ cell "STATUS" : map (cell . either (fromString . displayException) (const "OK") . snd) result
  col3 = fromList $ cell "FEED" : map (cell . show . pretty . fst) result
  sep = fromList [separator mempty 2]
