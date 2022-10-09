{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Implementation of "Imm.Logger" based on @fast-logger@.
module Logger (withLogHandler, module Reexport) where

-- {{{ Imports
import Imm.Logger as Reexport
import Imm.Pretty
import Prettyprinter.Render.Terminal
import System.Directory
import System.FilePath
import System.Log.FastLogger (FormattedTime, LogStr, LogType' (..), TimedFastLogger, ToLogStr (..), defaultBufSize, newTimeCache, simpleTimeFormat', withTimedFastLogger)

-- }}}

mkHandle :: MonadIO m => TimedFastLogger -> IO (Handle m)
mkHandle logAction = do
  logLevel <- newTVarIO Debug
  return $
    Handle
      (myLog logLevel logAction)
      (readTVarIO logLevel)
      (atomically . writeTVar logLevel)

myLog :: MonadIO m => TVar LogLevel -> TimedFastLogger -> LogLevel -> Doc AnsiStyle -> m ()
myLog minLogLevelTVar logAction logLevel document = io $ do
  minLogLevel <- readTVarIO minLogLevelTVar
  when (logLevel >= minLogLevel) $ logAction (\time -> formatLog time logLevel document)

formatLog :: FormattedTime -> LogLevel -> Doc AnsiStyle -> LogStr
formatLog time logLevel message =
  toLogStr $
    renderStrict $
      layoutPretty defaultLayoutOptions $
        pretty (decodeUtf8 time :: Text) <+> pretty logLevel <+> message <> hardline

withLogHandler :: MonadIO m => (Handle m -> IO a) -> IO a
withLogHandler f = do
  logFile <- getXdgDirectory XdgConfig $ "imm" </> "imm.log"
  timeCache <- newTimeCache simpleTimeFormat'
  withTimedFastLogger timeCache (LogFileNoRotate logFile defaultBufSize) $ f <=< mkHandle
