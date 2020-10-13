{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- | Implementation of "Imm.Logger" based on @co-log@.
module Logger (withLogHandler, module Reexport) where

-- {{{ Imports
import           Imm.Logger                                as Reexport
import           Imm.Pretty

import           Chronos
import           Colog                                     hiding (Severity (..))
import qualified Colog
import           Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.TypeRepMap                           as TypeRepMap
import           System.Directory
import           System.FilePath
-- }}}

mkHandle :: MonadIO m => MonadIO m' => LogAction m (RichMsg m' (Msg Colog.Severity)) -> IO (Handle m)
mkHandle logAction = do
  logLevel <- newTVarIO Debug
  return $ Handle
    (myLog logAction)
    (readTVarIO logLevel)
    (atomically . writeTVar logLevel)


myLog :: MonadIO m' => MonadIO m => LogAction m' (RichMsg m (Msg Colog.Severity)) -> LogLevel -> Doc AnsiStyle -> m' ()
myLog logAction logLevel document = logAction <& RichMsg (Msg (level2severity logLevel) callStack message) defaultFieldMap where
  level2severity Debug   = Colog.Debug
  level2severity Info    = Colog.Info
  level2severity Warning = Colog.Warning
  level2severity Error   = Colog.Error
  message = renderStrict $ layoutPretty defaultLayoutOptions document


withLogHandler :: MonadIO m => (Handle m -> IO a) -> IO a
withLogHandler f = do
  logFile <- getXdgDirectory XdgConfig $ "imm" </> "imm.log"
  withLogByteStringFile logFile $ \logFileAction ->
    withBackgroundLogger defCapacity logFileAction $ \logAction ->
      f =<< mkHandle (cmapM serializeMessage logAction)
  where serializeMessage = fmap encodeUtf8 . formatMessage

formatMessage :: Monad m => RichMsg m (Msg Colog.Severity) -> m Text
formatMessage (RichMsg (Msg severity stack text) fields) = do
  posixTime <- extractField $ TypeRepMap.lookup @"posixTime" fields
  threadId <- extractField $ TypeRepMap.lookup @"threadId" fields
  return $ showSeverity severity
    <> " " <> maybe mempty showTime posixTime
    <> " " <> showSourceLoc stack
    <> " " <> showThread threadId
    <> " " <> text
  where showTime posixTime = "[" <> encode_YmdHMS (SubsecondPrecisionFixed 0) hyphen (timeToDatetime posixTime)  <> "]"
        showThread (Just tid) = "[" <> show tid <> "]"
        showThread _ = mempty
