  {-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
import qualified Imm.Database             as Database
import           Imm.Database.FeedTable
import qualified Imm.Database.FeedTable   as Database
import           Imm.Feed
import           Imm.Hooks                as Hooks
import           Imm.HTTP                 (HttpClientF)
import qualified Imm.HTTP                 as HTTP
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty
import           Imm.XML
-- import Control.Concurrent.Async.Lifted (Async, async, mapConcurrently, waitAny)
-- import Control.Concurrent.Async.Pool
import           Control.Monad.Free
import qualified Data.ByteString          as ByteString
import           Data.Conduit
import           Data.Conduit.Combinators as Conduit (stdin)
import qualified Data.Map                 as Map
import           Data.NonNull
import           Data.Set                 (Set)
import           Data.Time.Format
import           Data.Tree
import           Data.Version
import qualified Paths_imm                as Package
import           Rainbow                  (chunk, chunksToByteStrings,
                                           toByteStringsColors256)
import           Rainbox
import           System.Info
import           Text.OPML.Conduit.Parse
import           Text.OPML.Types          as OPML
import           Text.XML                 as XML ()
import           Text.XML.Stream.Parse    as XML
import           URI.ByteString
-- }}}


printVersions :: (MonadIO m) => m ()
printVersions = io $ do
  putStrLn $ "imm-" ++ showVersion Package.version
  putStrLn $ "compiled by " ++ compilerName ++ "-" ++ showVersion compilerVersion

-- | Print database status for given feed(s)
showFeed :: (MonadIO m, LoggerF :<: f, MonadThrow m, MonadFree (SumF f) m, DatabaseF' :<: f)
         => [FeedID] -> m ()
showFeed feedIDs = do
  feeds <- Database.fetchList FeedTable feedIDs
  flushLogs
  if null feeds then logWarning "No subscription" else putBox $ entryTableToBox feeds

-- | Register the given feed URI in database
subscribe :: (LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, MonadCatch m)
          => URI -> Maybe Text -> m ()
subscribe uri category = Database.register (FeedID uri) $ fromMaybe "default" category

-- | Check for unread elements without processing them
check :: (MonadIO m, MonadCatch m, LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, HttpClientF :<: f, XmlParserF :<: f)
      => [FeedID] -> m ()
check feedIDs = do
  results <- for (zip ([1..] :: [Int]) feedIDs) $ \(i, feedID) -> do
    logInfo $ brackets (fill width (bold $ cyan $ pretty i) <+> "/" <+> pretty total) <+> "Checking" <+> magenta (pretty feedID) <> "..."
    try $ checkOne feedID

  flushLogs

  putBox $ statusTableToBox $ mapFromList $ zip feedIDs results

  let (failures, successes) = partitionEithers $ zipWith (\a -> bimap (a,) (a,)) feedIDs results
  unless (null failures) $ logError $ bold (pretty $ length failures) <+> "feeds in error"
  forM_ failures $ \(feedID, e) ->
    logError $ indent 2 (pretty feedID <++> indent 2 (pretty $ displayException e))

  where width = length (show total :: String)
        total = length feedIDs

checkOne :: (MonadIO m, MonadCatch m, LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, HttpClientF :<: f, XmlParserF :<: f)
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


run :: (MonadIO m, MonadCatch m, HooksF :<: f, LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, HttpClientF :<: f, XmlParserF :<: f)
    => [FeedID] -> m ()
run feedIDs = do
  results <- for (zip ([1..] :: [Int]) feedIDs) $ \(i, feedID) -> do
    logInfo $ brackets (fill width (bold $ cyan $ pretty i) <+> "/" <+> pretty total) <+> "Processing" <+> magenta (pretty feedID) <> "..."
    result <- tryAny $ runOne feedID
    return $ bimap (feedID,) (feedID,) result

  flushLogs

  let (failures, successes) = partitionEithers results

  unless (null failures) $ logError $ bold (pretty $ length failures) <+> "feeds in error"
  forM_ failures $ \(feedID, e) ->
    logError $ indent 2 (pretty feedID <++> indent 2 (pretty $ displayException e))

  where width = length (show total :: String)
        total = length feedIDs

runOne :: (MonadIO m, MonadCatch m, HooksF :<: f, LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, HttpClientF :<: f, XmlParserF :<: f)
    => FeedID -> m ()
runOne feedID = do
  feed <- getFeed feedID
  unreadElements <- filterM (fmap not . isRead feedID) $ getElements feed

  unless (null unreadElements) $ logInfo $ indent 2 $ green (pretty $ length unreadElements) <+> "new element(s)"

  forM_ unreadElements $ \element -> do
    onNewElement feed element
    mapM_ (Database.addReadHash feedID) $ getHashes element

  Database.markAsRead feedID


isRead :: (MonadCatch m, DatabaseF' :<: f, MonadFree (SumF f) m) => FeedID -> FeedElement -> m Bool
isRead feedID element = do
  DatabaseEntry _ _ readHashes lastCheck <- Database.fetch FeedTable feedID
  let matchHash = not $ null $ (setFromList (getHashes element) :: Set Int) `intersection` readHashes
      matchDate = case (lastCheck, getDate element) of
        (Nothing, _)     -> False
        (_, Nothing)     -> False
        (Just a, Just b) -> a > b
  return $ matchHash || matchDate

-- | 'subscribe' to all feeds described by the OPML document provided in input (stdin)
importOPML :: (MonadIO m, LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, MonadCatch m) => m ()
importOPML = do
  opml <- runConduit $ Conduit.stdin =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml
  forM_ (opmlOutlines opml) $ importOPML' mempty

importOPML' :: (MonadIO m, LoggerF :<: f, MonadFree (SumF f) m, DatabaseF' :<: f, MonadCatch m)
            => Maybe Text -> Tree OpmlOutline -> m ()
importOPML' _ (Node (OpmlOutlineGeneric b _) sub) = mapM_ (importOPML' (Just . toNullable $ OPML.text b)) sub
importOPML' c (Node (OpmlOutlineSubscription _ s) _) = subscribe (xmlUri s) c
importOPML' _ _ = return ()


getFeed :: (MonadIO m, MonadCatch m, MonadFree (SumF f) m, HttpClientF :<: f, LoggerF :<: f, XmlParserF :<: f)
        => FeedID -> m Feed
getFeed (FeedID uri) = HTTP.get uri >>= parseXml uri


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
  format = maybe "<never>" (fromString . formatTime defaultTimeLocale "%F %R")
  sep = fromList [separator mempty 1]


type StatusTable = Map FeedID (Either SomeException Int)

statusTableToBox :: StatusTable -> Box Horizontal
statusTableToBox t = tableByColumns $ Rainbox.intersperse sep $ fromList [col1, col2, col3] where
  result = sortBy (comparing fst) $ Map.toList t
  col1 = fromList $ cell "# UNREAD" : map (cell . either (const "?") show . snd) result
  col2 = fromList $ cell "STATUS" : map (cell . either (const "ERROR") (const "OK") . snd) result
  col3 = fromList $ cell "FEED" : map (cell . show . pretty . fst) result
  sep = fromList [separator mempty 2]
