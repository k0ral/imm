{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
-- {{{ Imports
import           Alternate
import qualified Core
import           Database
import           HTTP
import           Logger
import           Options
import           XML

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TMChan
import           Control.Exception.Safe
import           Data.Conduit.Combinators      as Conduit (stdin)
import qualified Data.MessagePack              as MsgPack
import           Dhall
import           Imm
import           Imm.Database.Feed             as Database
import qualified Imm.HTTP                      as HTTP
import           Imm.Pretty
import           Pipes.ByteString
import           System.Exit
import           System.Process.Typed
import           URI.ByteString
-- }}}


main :: IO ()
main = do
  AllOptions{..} <- parseOptions
  let GlobalOptions{..} = optionGlobal

  -- Setup logger
  logger <- Logger.mkHandle <$> defaultLogger
  setColorizeLogs logger optionColorizeLogs
  setLogLevel logger optionLogLevel
  log logger Debug $ "Options: " <> pretty optionCommand

  handleAny (log logger Error . pretty . displayException) $ do
    -- Setup database
    database <- Database.mkHandle <$> defaultDatabase
    let database' = if optionReadOnlyDatabase then readOnly logger database else database
    log logger Debug . ("Using database:" <++>) . indent 2 =<< _describeDatabase database'

    case optionCommand of
      Import            -> Core.importOPML logger database' Conduit.stdin
      Subscribe u c     -> Core.subscribe logger database' u c
      Unsubscribe query -> Core.unsubscribe logger database' query
      List              -> Core.listFeeds logger database'
      Describe query    -> Core.describeFeed logger database query
      Reset feedKeys    -> Core.markAsUnprocessed logger database' feedKeys
      Run f c           -> main2 logger database' f =<< resolveCallbacks c optionCallbacksFile

    Database.commit logger database'

  flushLogs logger


resolveCallbacks :: MonadIO m => CallbackMode -> FilePath -> m [Callback]
resolveCallbacks EnableCallbacks callbacksFile = io $ input auto $ fromString callbacksFile
resolveCallbacks _ _                           = return mempty


main2 :: Logger.Handle IO -> Database.Handle IO -> FeedQuery -> [Callback] -> IO ()
main2 logger database feedQuery callbacks = do
  let httpClient = HTTP.mkHandle logger

  feedLocations <- mapM (Database.resolveFeedLocation database)
    =<< Database.resolveEntryKey database feedQuery

  feedLocationsChan <- newTMChanIO
  targetFeedChan <- newTMChanIO
  newItemsChan <- newTMChanIO
  processedChan <- newTMChanIO

  resolveErrorsChan <- newTMChanIO
  fetchErrorsChan <- newTMChanIO
  runErrorsChan <- newTMChanIO
  callbackErrorsChan <- newTMChanIO

  newItemsCount <- newTVarIO (0 :: Int)
  errorsCount <- newTVarIO (0 :: Int)

  -- => Feed IDs events
  producer <- async $ forM_ feedLocations $ atomically . writeTMChan feedLocationsChan

  -- Feed locations => feed direct URIs
  let resolverF feedLocation = do
        uri <- resolveFeedURI logger httpClient feedLocation
        atomically $ writeTMChan targetFeedChan (feedLocation, uri)

  resolvers <- spawnConsumers logger 5 feedLocationsChan resolverF resolveErrorsChan errorsCount

  -- Feed direct URIs events => new item events
  let fetcherF (feedLocation, uri) = do
        feed <- HTTP.withGet logger httpClient uri
          $ toLazyM >=> parseXml xmlParser uri
        let entryKey = ByLocation feedLocation

        unreadElements <- filterM (fmap not . isRead database entryKey) $ getElements feed
        unprocessedElements <- listUnprocessedElements database entryKey
        forM_ (unprocessedElements <> unreadElements) $ \element -> do
          log logger Info $ "New item:" <+> magenta (pretty entryKey) <+> "/" <+> yellow (pretty $ getTitle element)
          atomically $ do
            writeTMChan newItemsChan (entryKey, removeElements feed, element)
            modifyTVar' newItemsCount (+ 1)

  fetchers <- spawnConsumers logger 5 targetFeedChan fetcherF fetchErrorsChan errorsCount

  -- New items events => execute callback => processed/error events
  let runnerF (entryKey, feed, element) = do
        results <- forM callbacks $ \callback@(Callback executable arguments) -> do
          let processInput = byteStringInput $ MsgPack.pack $ Message feed element
              processConfig = proc executable (toString <$> arguments) & setStdin processInput

          log logger Debug $ "Running" <+> cyan (pretty executable) <+> "on" <+> magenta (pretty entryKey) <+> "/" <+> yellow (pretty $ getTitle element)

          (exitCode, output, errors) <- readProcess processConfig
          case exitCode of
            ExitSuccess   -> return $ Right callback
            ExitFailure i -> return $ Left (callback, i, output, errors)

        case lefts results of
          [] -> atomically $ writeTMChan processedChan (entryKey, element)
          e  -> atomically $ do
            writeTMChan callbackErrorsChan ((entryKey, element), e)
            modifyTVar' errorsCount (+ 1)

  runners <- spawnConsumers logger 5 newItemsChan runnerF runErrorsChan errorsCount

  -- Processed events => update database
  storer <- async $ fix $ \recurse -> do
    item <- atomically $ readTMChan processedChan
    forM_ item $ \(feedID, element) -> do
      log logger Debug $ "Updating database for" <+> pretty feedID <+> "/" <+> pretty (getTitle element)
      Database.markAsProcessed logger database feedID element
      recurse

  -- Wait for all async processes to complete
  wait producer
  atomically $ closeTMChan feedLocationsChan

  mapM_ wait resolvers
  atomically $ do
    closeTMChan targetFeedChan
    closeTMChan resolveErrorsChan

  mapM_ wait fetchers
  atomically $ closeTMChan newItemsChan

  mapM_ wait runners
  atomically $ do
    closeTMChan processedChan
    closeTMChan fetchErrorsChan
    closeTMChan callbackErrorsChan
    closeTMChan runErrorsChan

  wait storer

  -- Error events => log
  handleErrors resolveErrorsChan (printResolveError logger)
  handleErrors fetchErrorsChan (printFetchError logger)
  handleErrors callbackErrorsChan (printCallbackError logger)
  handleErrors runErrorsChan (printRunError logger)
  flushLogs logger

  readTVarIO newItemsCount <&> pretty <&> bold <&> (<+> "new items") >>= log logger Info
  readTVarIO errorsCount <&> pretty <&> bold <&> (<+> "errors") >>= log logger Info


spawnConsumers :: Logger.Handle IO -> Int -> TMChan i -> (i -> IO ()) -> TMChan (i, SomeException) -> TVar Int -> IO [Async ()]
spawnConsumers logger n inputChan f errorsChan errorsCount =
  replicateM n $ async $ fix $ \recurse -> do
    maybeItem <- atomically $ readTMChan inputChan
    forM_ maybeItem $ \item -> do
      catchAny (f item) $ \e -> do
        log logger Debug $ "Error:" <+> pretty (displayException e)
        atomically $ do
          writeTMChan errorsChan (item, e)
          modifyTVar' errorsCount (+ 1)
      recurse

handleErrors :: MonadIO m => TMChan (input, error) -> ((input, error) -> m ()) -> m ()
handleErrors errorsChan handler = fix $ \recurse -> do
  items <- atomically $ readTMChan errorsChan
  forM_ items $ \(i, e) -> handler (i, e) >> recurse

printResolveError :: Exception e => Logger.Handle m -> (FeedLocation, e) -> m ()
printResolveError logger (feedLocation, e) = log logger Error $
  bold ("Resolve error for" <+> pretty feedLocation)
  <++> indent 2 (pretty $ displayException e)

printFetchError :: Exception e => Logger.Handle m -> ((FeedLocation, URIRef a), e) -> m ()
printFetchError logger ((_, uri), e) = log logger Error $
  bold ("Fetch error for" <+> prettyURI uri)
  <++> indent 2 (pretty $ displayException e)

printCallbackError :: Logger.Handle m -> ((EntryKey, FeedElement), _) -> m ()
printCallbackError logger ((entryKey, element), e) = log logger Error $
  bold ("Callback error for" <+> pretty entryKey <+> "/" <+> pretty (getTitle element))
  <++> indent 2 prettyErrors
  where prettyErrors = vsep $ do
          (callback, i, stdout', stderr') <- e
          return $ "When running:" <+> pretty callback
            <++> "Exit code:" <+> pretty i
            <++> "Stdout:" <++> indent 2 (pretty $ decodeUtf8 @Text stdout')
            <++> "Stderr:" <++> indent 2 (pretty $ decodeUtf8 @Text stderr')

printRunError :: Exception e => Logger.Handle m -> ((EntryKey, Feed, FeedElement), e) -> m ()
printRunError logger ((entryKey, _feed, element), e) = log logger Error $
  bold ("Error for" <+> pretty entryKey <+> "/" <+> pretty (getTitle element))
  <++> indent 2 (pretty $ displayException e)


xmlParser :: XML.Handle IO
xmlParser = XML.mkHandle defaultXmlParser
