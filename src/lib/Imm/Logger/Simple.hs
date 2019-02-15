{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Implementation of "Imm.Logger" based on @fast-logger@.
-- For further information, please consult "System.Log.FastLogger".
module Imm.Logger.Simple (module Imm.Logger.Simple, module Reexport) where

-- {{{ Imports
import           Imm.Logger                                as Reexport
import           Imm.Pretty

import           Data.Text.Prettyprint.Doc.Render.Terminal
import           System.Log.FastLogger                     as Reexport
-- }}}

data LoggerSettings = LoggerSettings
  { _loggerSet      :: LoggerSet       -- ^ 'LoggerSet' used for 'Debug', 'Info' and 'Warning' logs
  , _errorLoggerSet :: LoggerSet       -- ^ 'LoggerSet' used for 'Error' logs
  , _logLevel       :: MVar LogLevel   -- ^ Discard logs that are strictly less serious than this level
  , _colorizeLogs   :: MVar Bool       -- ^ Enable log colorisation
  }


-- | Default logger forwards error messages to stderr, and other messages to stdout.
defaultLogger :: IO LoggerSettings
defaultLogger = LoggerSettings
  <$> newStdoutLoggerSet defaultBufSize
  <*> newStderrLoggerSet defaultBufSize
  <*> newMVar Info
  <*> newMVar True


mkHandle :: MonadIO m => LoggerSettings -> Handle m
mkHandle settings = Handle
  { log = \l t -> do
      refLevel <- readMVar $ _logLevel settings
      handleColor <- (\c -> if c then id else unAnnotate) <$> readMVar (_colorizeLogs settings)
      let loggerSet = (if l == Error then _errorLoggerSet else _loggerSet) settings
      when (l >= refLevel) $ liftIO $ pushLogStrLn loggerSet $ toLogStr $ renderLazy $ layoutPretty defaultLayoutOptions $ handleColor t

  , getLogLevel = readMVar $ _logLevel settings
  , setLogLevel = void . swapMVar (_logLevel settings)
  , setColorizeLogs = void . swapMVar (_colorizeLogs settings)
  , flushLogs = liftIO $ do
      flushLogStr $ _loggerSet settings
      flushLogStr $ _errorLoggerSet settings
  }
