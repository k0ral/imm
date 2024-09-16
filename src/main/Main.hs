{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- {{{ Imports

import Control.Concurrent.STM.TMChan
import Control.Exception.Safe
import qualified Core
import Database.Async as Database
import Database.Handle as Database
import Database.ReadOnly as Database
import Database.SQLite as SQLite
import Dhall (auto, input)
import HTTP
import Imm
import Imm.Pretty
import Input
import Logger
import Output (putDocLn)
import qualified Output
import Pipes.ByteString hiding (filter, stdout)
import Safe
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Fold as Fold
import XML
import GHC.Conc (numCapabilities)

-- }}}

main ∷ IO ()
main = do
  programInput ← parseOptions
  Output.withHandle $ \stdout → do
    withLogger programInput $ \logger → do
      handleAny (log logger Error . pretty . displayException) $ do
        withDatabase logger stdout programInput $ \database → do
          case inputCommand programInput of
            Subscribe u c → Core.subscribe logger stdout database u c
            Unsubscribe query → Core.unsubscribe logger database query
            Describe query → Core.describeFeeds stdout database query
            Reset query → Core.markAsUnprocessed logger database query
            Run f c → main2 logger stdout database f =<< resolveCallbacks c (inputCallbacksFile programInput)

          Database.commit logger database

withLogger ∷ ProgramInput → (Logger.Handle IO → IO ()) → IO ()
withLogger programInput f = withLogHandler $ \logger → do
  setLogLevel logger $ inputLogLevel programInput
  log logger Info $ "Input:" <+> pretty programInput
  f logger

withDatabase ∷ m ~ IO ⇒ Logger.Handle m → Output.Handle m → ProgramInput → (Database.Handle m → m ()) → m ()
withDatabase logger stdout programInput f = do
  database ← SQLite.mkHandle =<< SQLite.defaultDatabase
  let database' = if inputReadOnlyDatabase programInput then readOnly logger database else database
  log logger Info . ("Using database:" <++>) . indent 2 =<< _describeDatabase database'
  Database.withAsyncHandle logger stdout database' f

resolveCallbacks ∷ MonadIO m ⇒ CallbackMode → FilePath → m [Callback]
resolveCallbacks EnableCallbacks callbacksFile = io $ input auto $ fromString callbacksFile
resolveCallbacks _ _ = return mempty

main2
  ∷ Logger.Handle IO
  → Output.Handle IO
  → Database.Handle IO
  → FeedQuery
  → [Callback]
  → IO ()
main2 logger stdout database feedQuery callbacks = do
  newItemsCount ← newTVarIO (0 ∷ Int)
  errorsCount ← newTVarIO (0 ∷ Int)
  errorsChan ← newTMChanIO

  httpClient ← HTTP.mkHandle

  -- Feed record => (feed record, items)
  let fetcher = catchErrors logger errorsChan errorsCount $ \feedRecord → do
        log logger Debug "Fetch worker starts"

        let location@(FeedLocation uri _) = _feedLocation feedRecord
        (feedDefinition, items) ← Core.downloadFeed logger httpClient (toLazyM >=> parseXml xmlParser uri) location

        newFeedStatus ← touchFeed $ _feedStatus feedRecord

        let newFeedRecord = feedRecord{_feedDefinition = feedDefinition, _feedStatus = newFeedStatus}
        _updateFeedDefinition database newFeedRecord
        _updateFeedStatus database newFeedRecord
        return $ Just (newFeedRecord, items)

  -- (Feed record, items) => (feed record, items, item records)
  let itemRetriever = catchErrors logger errorsChan errorsCount $ \(feedRecord, items) → do
        itemRecords ← _fetchItems database $ _feedKey feedRecord
        return $ Just (feedRecord, items, itemRecords)

  -- (Feed record, items, item records) => multiple (feed record, item, item records)
  let itemSplitter (feedRecord, items, itemRecords) =
        Stream.fromList [(feedRecord, item, itemRecords) | item ← items]

  let itemMatcher (feedRecord, item, itemRecords) = do
        let itemRecord = itemRecords & filter (\r → _itemDefinition r `areSameItem` item) & headMay
        return (feedRecord, item, itemRecord)

  -- Filter for unread items
  let unreadSelector (feedRecord, item, itemRecord) = checkDate || checkStatus
       where
        checkDate = case (_feedLastUpdate $ _feedStatus feedRecord, _itemDate item) of
          (Nothing, _) → True
          (Just t0, Just t1) → t0 < t1
          _ → False
        checkStatus = itemRecord <&> _itemStatus <&> (not . _isProcessed) & fromMaybe True
  -- unprocessedElements <- listUnprocessedElements database entryKey

  let logNewItem (feedRecord, item, _) =
        putDocLn stdout $
          "New item:"
            <+> maybe "<unknown>" prettyTime (_itemDate item)
            <+> magenta (prettyName feedRecord)
            <+> "/"
            <+> yellow (prettyName item)

  -- New items events => execute callback => processed/error events
  let runner = catchErrors logger errorsChan errorsCount $ \(feedRecord, item, itemRecord) → do
        results ← forM callbacks $ \callback → io $ do
          runCallback logger callback $ CallbackMessage (_feedLocation feedRecord) (_feedDefinition feedRecord) item

        case lefts results of
          [] → return $ Just (feedRecord, item, itemRecord)
          e → do
            io $
              atomically $ do
                writeTMChan errorsChan (toException $ CallbackException feedRecord item e)
                modifyTVar' errorsCount (+ 1)
            return Nothing

  let storer (_, _, Just itemRecord) =
        Database.markItemAsProcessed logger database itemRecord
      storer (feedRecord, item, _) = do
        let itemRecord = mkFeedItemRecord (_feedKey feedRecord) item (FeedItemStatus True)
        void $ Database.insertItem logger database itemRecord

  entries ← case feedQuery of
    QueryAll → Database._fetchAllFeeds database
    QueryByUID uid → pure <$> Database._fetchFeed database uid

  let streamConfig = Stream.maxThreads numCapabilities . Stream.maxBuffer 100

  Stream.fromList entries
    & Stream.parMapM streamConfig fetcher
    & Stream.catMaybes
    & Stream.parMapM streamConfig itemRetriever
    & Stream.catMaybes
    & Stream.parConcatMap streamConfig itemSplitter
    & Stream.parMapM streamConfig itemMatcher
    & Stream.filter unreadSelector
    & Stream.trace logNewItem
    & Stream.trace (const $ atomically $ modifyTVar' newItemsCount (+ 1))
    & Stream.parMapM streamConfig runner
    & Stream.catMaybes
    & Stream.mapM storer
    & Stream.fold Fold.drain

  atomically $ closeTMChan errorsChan

  -- Error events => log
  handleErrors stdout errorsChan

  readTVarIO newItemsCount <&> pretty <&> bold <&> (<+> "new items") >>= putDocLn stdout
  readTVarIO errorsCount <&> pretty <&> bold <&> (<+> "errors") >>= putDocLn stdout

data CallbackException = CallbackException (FeedRecord Inserted) FeedItem [(Callback, Int, LByteString, LByteString)]
  deriving (Eq, Generic, Ord, Show, Typeable)

instance Exception CallbackException where
  displayException = show . pretty

instance Pretty CallbackException where
  pretty (CallbackException feedRecord item e) =
    "Callback error for"
      <+> prettyName (_feedDefinition feedRecord)
      <+> "/"
      <+> prettyName item
        <++> indent 2 prettyErrors
   where
    prettyErrors = vsep $ do
      (callback, i, stdout', stderr') ← e
      return $
        "When running:"
          <+> pretty callback
            <++> "Exit code:"
          <+> pretty i
            <++> "Stdout:"
            <++> indent 2 (pretty $ decodeUtf8 @Text stdout')
            <++> "Stderr:"
            <++> indent 2 (pretty $ decodeUtf8 @Text stderr')

catchErrors
  ∷ MonadIO m
  ⇒ MonadCatch m
  ⇒ Logger.Handle m
  → TMChan SomeException
  → TVar Int
  → (i → m (Maybe a))
  → i
  → m (Maybe a)
catchErrors logger errorsChan errorsCount f input_ = catchAny (f input_) $ \e → do
  log logger Error $ pretty (displayException e)
  io $
    atomically $ do
      writeTMChan errorsChan e
      modifyTVar' errorsCount (+ 1)
  return Nothing

handleErrors ∷ MonadIO m ⇒ Output.Handle m → TMChan SomeException → m ()
handleErrors stdout errorsChan = fix $ \recurse → do
  items ← atomically $ readTMChan errorsChan
  forM_ items $ \e → putDocLn stdout (red $ pretty $ displayException e) >> recurse

xmlParser ∷ XML.Handle IO
xmlParser = XML.mkHandle defaultXmlParser
