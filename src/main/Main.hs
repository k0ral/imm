{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- {{{ Imports
import           Alternate
import qualified Core
import           Database
import           HTTP
import           Logger
import           Options
import           XML

import           Control.Concurrent.STM.TMChan
import           Control.Exception.Safe
import           Data.Conduit.Combinators      as Conduit (stdin)
import           Dhall
import           Imm
import qualified Imm.Callback                  as Callback
import           Imm.Database.Feed             as Database
import qualified Imm.HTTP                      as HTTP
import           Imm.Pretty
import           Pipes.ByteString
import           Streamly                      as Stream
import qualified Streamly.Prelude              as Stream
import           System.Exit
import           System.Process.Typed
import           URI.ByteString
-- }}}


main :: IO ()
main = withLogHandler $ \logger -> do
  AllOptions{..} <- parseOptions
  let GlobalOptions{..} = optionGlobal

  -- Setup logger
  setLogLevel logger optionLogLevel
  log logger Info $ "Global options:" <+> pretty optionGlobal
  log logger Info $ "Command:" <+> pretty optionCommand

  handleAny (log logger Error . pretty . displayException) $ do
    -- Setup database
    database <- Database.mkHandle <$> defaultDatabase
    let database' = if optionReadOnlyDatabase then readOnly logger database else database
    log logger Info . ("Using database:" <++>) . indent 2 =<< _describeDatabase database'

    case optionCommand of
      Import            -> Core.importOPML logger database' Conduit.stdin
      Subscribe u c     -> Core.subscribe logger database' u c
      Unsubscribe query -> Core.unsubscribe logger database' query
      List              -> Core.listFeeds logger database'
      Describe query    -> Core.describeFeed logger database query
      Reset feedKeys    -> Core.markAsUnprocessed logger database' feedKeys
      Run f c           -> main2 logger database' f =<< resolveCallbacks c optionCallbacksFile

    Database.commit logger database'


resolveCallbacks :: MonadIO m => CallbackMode -> FilePath -> m [Callback]
resolveCallbacks EnableCallbacks callbacksFile = io $ input auto $ fromString callbacksFile
resolveCallbacks _ _                           = return mempty


main2 :: Logger.Handle IO -> Database.Handle IO -> FeedQuery -> [Callback] -> IO ()
main2 logger database feedQuery callbacks = do
  resolveErrorsChan <- newTMChanIO
  fetchErrorsChan <- newTMChanIO
  runErrorsChan <- newTMChanIO
  callbackErrorsChan <- newTMChanIO

  newItemsCount <- newTVarIO (0 :: Int)
  errorsCount <- newTVarIO (0 :: Int)

  let httpClient = HTTP.mkHandle logger

  -- Feed locations => feed direct URIs
  let resolver = catchErrors logger resolveErrorsChan errorsCount (return Nothing) $ \feedLocation -> do
        uri <- io $ resolveFeedURI logger httpClient feedLocation
        return $ Just (feedLocation, uri)
  -- Feed direct URIs events => new item events
  let fetcherE = catchErrors logger fetchErrorsChan errorsCount (return []) $ \(feedLocation, uri) -> do
        log logger Debug "Fetch worker starts"
        feed <- HTTP.withGet logger httpClient uri $ toLazyM >=> parseXml xmlParser uri
        let entryKey = ByLocation feedLocation

        unreadElements <- filterM (fmap not . isRead database entryKey) $ getElements feed
        unprocessedElements <- listUnprocessedElements database entryKey
        return $ for (unprocessedElements <> unreadElements) (entryKey, removeElements feed,)
      fetcher i = do
        elements <- lift $ catchAny (fetcherE i) $ \e -> do
          log logger Error $ pretty (displayException e)
          atomically $ do
            writeTMChan fetchErrorsChan (i, e)
            modifyTVar' errorsCount (+ 1)
          return []
        Stream.fromList elements
  -- New items events => execute callback => processed/error events
  let runner = catchErrors logger runErrorsChan errorsCount (return Nothing) $ \(entryKey, feed, element) -> do
        Core.putDocLn $ "New item:" <+> magenta (pretty entryKey) <+> "/" <+> yellow (pretty $ getTitle element)
        atomically $ modifyTVar' newItemsCount (+ 1)

        results <- forM callbacks $ \callback@(Callback executable arguments) -> io $ do
          let processInput = byteStringInput $ Callback.serializeMessage feed element
              processConfig = proc executable (toString <$> arguments) & setStdin processInput

          log logger Info $ "Running" <+> cyan (pretty executable) <+> "on" <+> magenta (pretty entryKey) <+> "/" <+> yellow (pretty $ getTitle element)

          (exitCode, output, errors) <- readProcess processConfig
          case exitCode of
            ExitSuccess   -> return $ Right callback
            ExitFailure i -> return $ Left (callback, i, output, errors)

        case lefts results of
          [] -> return $ Just (entryKey, element)
          e  -> do
            io $ atomically $ do
              writeTMChan callbackErrorsChan ((entryKey, element), e)
              modifyTVar' errorsCount (+ 1)
            return Nothing
  let storer (feedID, element) = do
        log logger Info $ "Updating database for" <+> pretty feedID <+> "/" <+> pretty (getTitle element)
        Database.markAsProcessed logger database feedID element


  entries <- Database.resolveEntryKey database feedQuery

  Stream.fromList entries
    |& Stream.mapM (Database.resolveFeedLocation database)
    |& Stream.mapMaybeM resolver
    |& Stream.concatMapWith async fetcher
    |& Stream.mapMaybeM runner
    |& Stream.mapM storer
    & Stream.asyncly
    & Stream.drain

  atomically $ do
    closeTMChan resolveErrorsChan
    closeTMChan fetchErrorsChan
    closeTMChan callbackErrorsChan
    closeTMChan runErrorsChan

  -- Error events => log
  handleErrors resolveErrorsChan printResolveError
  handleErrors fetchErrorsChan printFetchError
  handleErrors callbackErrorsChan printCallbackError
  handleErrors runErrorsChan printRunError

  readTVarIO newItemsCount <&> pretty <&> bold <&> (<+> "new items") >>= Core.putDocLn
  readTVarIO errorsCount <&> pretty <&> bold <&> (<+> "errors") >>= Core.putDocLn


catchErrors :: MonadIO m => MonadCatch m => Logger.Handle m -> TMChan (i, SomeException) -> TVar Int -> m a -> (i -> m a) -> i -> m a
catchErrors logger errorsChan errorsCount fe f input_ = catchAny (f input_) $ \e -> do
  log logger Error $ pretty (displayException e)
  io $ atomically $ do
    writeTMChan errorsChan (input_, e)
    modifyTVar' errorsCount (+ 1)
  fe


handleErrors :: MonadIO m => TMChan (input, error) -> ((input, error) -> m ()) -> m ()
handleErrors errorsChan handler = fix $ \recurse -> do
  items <- atomically $ readTMChan errorsChan
  forM_ items $ \(i, e) -> handler (i, e) >> recurse

printResolveError :: Exception e => MonadIO m => (FeedLocation, e) -> m ()
printResolveError (feedLocation, e) = Core.putDocLn $ red $
  bold ("Resolve error for" <+> pretty feedLocation)
  <++> indent 2 (pretty $ displayException e)

printFetchError :: Exception e => MonadIO m => ((FeedLocation, URIRef a), e) -> m ()
printFetchError ((_, uri), e) = Core.putDocLn $ red $
  bold ("Fetch error for" <+> prettyURI uri)
  <++> indent 2 (pretty $ displayException e)

printCallbackError :: i ~ (EntryKey, FeedElement)
                   => e ~ (Callback, Int, LByteString, LByteString)
                   => MonadIO m
                   => (i, [e]) -> m ()
printCallbackError ((entryKey, element), e) = Core.putDocLn $ red $
  bold ("Callback error for" <+> pretty entryKey <+> "/" <+> pretty (getTitle element))
  <++> indent 2 prettyErrors
  where prettyErrors = vsep $ do
          (callback, i, stdout', stderr') <- e
          return $ "When running:" <+> pretty callback
            <++> "Exit code:" <+> pretty i
            <++> "Stdout:" <++> indent 2 (pretty $ decodeUtf8 @Text stdout')
            <++> "Stderr:" <++> indent 2 (pretty $ decodeUtf8 @Text stderr')

printRunError :: Exception e => MonadIO m => ((EntryKey, Feed, FeedElement), e) -> m ()
printRunError ((entryKey, _feed, element), e) = Core.putDocLn $ red $
  bold ("Error for" <+> pretty entryKey <+> "/" <+> pretty (getTitle element))
  <++> indent 2 (pretty $ displayException e)


xmlParser :: XML.Handle IO
xmlParser = XML.mkHandle defaultXmlParser
