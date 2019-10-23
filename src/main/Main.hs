{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
-- {{{ Imports
import           Imm
import           Imm.Database                  as Database
import qualified Imm.HTTP                      as HTTP
import           Imm.Pretty

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
import qualified Data.Map                      as Map
import qualified Data.MessagePack              as MsgPack
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Dhall
import           Relude.Unsafe                 (at)
import           System.Exit
import           System.IO                     (hFlush)
import           System.Process.Typed
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
      Import           -> Core.importOPML logger database' Conduit.stdin
      Subscribe u c    -> Core.subscribe logger database' u c
      List             -> Core.listFeeds logger database'
      ShowFeed feedRef -> Core.showFeed logger database =<< toFeedID database' feedRef
      OnFeedRef t c    -> main2 logger database' optionCallbacksFile t c

    Database.commit logger database'

  flushLogs logger


main2 :: Logger.Handle IO -> Database.Handle IO FeedTable -> FilePath -> Maybe FeedRef -> CommandOnFeedRef -> IO ()
main2 logger database callbacksFile feedRef command = do
  feedIDs <- resolveTarget database ByPassConfirmation feedRef

  case command of
    Unsubscribe      -> Database.deleteList logger database feedIDs
    Reset            -> mapM_ (Database.markAsUnprocessed logger database) feedIDs
    Run callbackMode -> do
      callbacks <- case callbackMode of
        EnableCallbacks -> input auto $ fromString callbacksFile
        _               -> return []
      main3 logger database callbacks feedIDs


main3 :: Logger.Handle IO -> Database.Handle IO FeedTable -> [Callback] -> [FeedID] -> IO ()
main3 logger database callbacks feedIDs = do
  httpClient <- HTTP.mkHandle <$> defaultManager

  targetFeedChan <- newTMChanIO
  newItemsChan <- newTMChanIO
  processedChan <- newTMChanIO
  fetchErrorsChan <- newTMChanIO
  callbackErrorsChan <- newTMChanIO

  newItemsCount <- newTVarIO (0 :: Int)
  errorsCount <- newTVarIO (0 :: Int)

  -- => Feed IDs events
  producer <- async $ forM_ feedIDs $ atomically . writeTMChan targetFeedChan

  -- Feed IDs events => new item events
  fetchers <- replicateM 5 $ async $ fix $ \recurse -> do
    target <- atomically $ readTMChan targetFeedChan
    forM_ target $ \feedID@(FeedID uri) -> do
      catchAny
        (do
          feed <- HTTP.get logger httpClient uri >>= parseXml xmlParser uri

          unreadElements <- filterM (fmap not . isRead database feedID) $ getElements feed
          unprocessedElements <- listUnprocessedElements database feedID
          forM_ (unprocessedElements <> unreadElements) $ \element -> do
            log logger Info $ "New item:" <+> magenta (pretty feedID) <+> "/" <+> yellow (pretty $ getTitle element)
            atomically $ do
              writeTMChan newItemsChan (feedID, removeElements feed, element)
              count <- readTVar newItemsCount
              writeTVar newItemsCount (count + 1) )

        (\e -> atomically $ do
          writeTMChan fetchErrorsChan (feedID, e)
          count <- readTVar errorsCount
          writeTVar errorsCount (count + 1) )

      recurse

  -- New items events => execute callback => processed/error events
  runners <- replicateM 5 $ async $ fix $ \recurse -> do
    newItem <- atomically $ readTMChan newItemsChan
    forM_ newItem $ \(feedID, feed, element) -> do
      results <- forM callbacks $ \callback@(Callback executable arguments) -> do
        let processInput = byteStringInput $ MsgPack.pack $ Message feed element
            processConfig = proc executable (toString <$> arguments) & setStdin processInput

        log logger Debug $ "Running" <+> cyan (pretty executable) <+> "on" <+> magenta (pretty feedID) <+> "/" <+> yellow (pretty $ getTitle element)

        (exitCode, output, errors) <- readProcess processConfig
        case exitCode of
          ExitSuccess   -> return $ Right callback
          ExitFailure i -> return $ Left (callback, i, output, errors)

      case lefts results of
        [] -> atomically $ writeTMChan processedChan (feedID, element)
        e  -> atomically $ do
          writeTMChan callbackErrorsChan (feedID, element, e)
          count <- readTVar errorsCount
          writeTVar errorsCount (count + 1)

      recurse

  -- Processed events => update database
  storer <- async $ fix $ \recurse -> do
    item <- atomically $ readTMChan processedChan
    forM_ item $ \(feedID, element) -> do
      log logger Debug $ "Updating database for" <+> pretty feedID <+> "/" <+> pretty (getTitle element)
      Database.markAsProcessed logger database feedID element
      recurse

  -- Wait for all async processes to complete
  wait producer
  atomically $ closeTMChan targetFeedChan

  mapM_ wait fetchers
  atomically $ closeTMChan newItemsChan

  mapM_ wait runners
  atomically $ do
    closeTMChan processedChan
    closeTMChan fetchErrorsChan
    closeTMChan callbackErrorsChan

  wait storer

  -- Error events => log
  printErrors logger fetchErrorsChan callbackErrorsChan
  flushLogs logger

  readTVarIO newItemsCount <&> pretty <&> bold <&> (<+> "new items") >>= log logger Info
  readTVarIO errorsCount <&> pretty <&> bold <&> (<+> "errors") >>= log logger Info

printErrors :: (MonadIO m, Exception e, Pretty a1, Pretty a2, Pretty a3, Pretty a4, ConvertUtf8 Text b1, ConvertUtf8 Text b2)
            => Logger.Handle m -> TMChan (a1, e) -> TMChan (a2, FeedElement, [(a3, a4, b1, b2)]) -> m ()
printErrors logger fetchErrorsChan callbackErrorsChan = do
  fix $ \recurse -> do
    items <- atomically $ readTMChan fetchErrorsChan
    forM_ items $ \(feedID, e) -> do
      log logger Error $ bold ("Fetch error for" <+> pretty feedID) <++> indent 2 (pretty $ displayException e)
      recurse

  fix $ \recurse -> do
    items <- atomically $ readTMChan callbackErrorsChan
    forM_ items $ \(feedID, element, errors) -> do
      let prettyErrors = vsep $ do
            (callback, i, stdout', stderr') <- errors
            return $ "When running:" <+> pretty callback
              <++> "Exit code:" <+> pretty i
              <++> "Stdout:" <++> indent 2 (pretty $ decodeUtf8 @Text stdout')
              <++> "Stderr:" <++> indent 2 (pretty $ decodeUtf8 @Text stderr')
      log logger Error $ bold ("Callback error for" <+> pretty feedID <+> "/" <+> pretty (getTitle element)) <++> indent 2 prettyErrors
      recurse


xmlParser :: XML.Handle IO
xmlParser = XML.mkHandle defaultXmlParser


-- * Util

data SafeGuard = AskConfirmation | ByPassConfirmation
  deriving(Eq, Read, Show)

data InterruptedException = InterruptedException deriving(Eq, Read, Show)
instance Exception InterruptedException where
  displayException _ = "Process interrupted"

promptConfirm :: Text -> IO ()
promptConfirm s = do
  Text.putStr $ s <> " Confirm [Y/n] "
  hFlush stdout
  x <- Text.getLine
  unless (Text.null x || x == "Y") $ throwM InterruptedException


resolveTarget :: MonadIO m => MonadThrow m => Database.Handle m FeedTable -> SafeGuard -> Maybe FeedRef -> m [FeedID]
resolveTarget database s Nothing = do
  result <- Map.keys <$> Database.fetchAll database
  when (s == AskConfirmation) $ liftIO $ promptConfirm $ "This will affect " <> show (length result) <> " feeds."
  return result
resolveTarget database _ (Just feedRef) = do
  result <- toFeedID database feedRef
  return [result]

toFeedID :: Monad m => Database.Handle m FeedTable -> FeedRef -> m FeedID
toFeedID database (ByUID i) = fst . at (i-1) . Map.toList <$> Database.fetchAll database
toFeedID _ (ByURI uri) = return $ FeedID uri
