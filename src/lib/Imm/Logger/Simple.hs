{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Implementation of "Imm.Logger" based on @fast-logger@.
-- For further information, please consult "System.Log.FastLogger".
module Imm.Logger.Simple (module Imm.Logger.Simple, module Reexport) where

-- {{{ Imports
import           Imm.Logger                                as Reexport
import           Imm.Prelude
import           Imm.Pretty

import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans.Reader
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           System.Log.FastLogger                     as Reexport
-- }}}

data LoggerSettings = LoggerSettings
  { _loggerSet      :: LoggerSet  -- ^ 'LoggerSet' used for 'Debug', 'Info' and 'Warning' logs
  , _errorLoggerSet :: LoggerSet  -- ^ 'LoggerSet' used for 'Error' logs
  , _logLevel       :: LogLevel   -- ^ Discard logs that are strictly less serious than this level
  , _colorizeLogs   :: Bool       -- ^ Enable log colorisation
  }

-- | Default logger forwards error messages to stderr, and other messages to stdout.
defaultLogger :: IO (MVar LoggerSettings)
defaultLogger = newMVar =<< LoggerSettings
  <$> newStdoutLoggerSet defaultBufSize
  <*> newStderrLoggerSet defaultBufSize
  <*> pure Info
  <*> pure True

instance MonadLog (ReaderT (MVar LoggerSettings) IO) where
  -- log :: LogLevel -> Doc -> m ()
  log l t = do
    settings <- readMVar =<< ask
    let loggerSet = (if l == Error then _errorLoggerSet else _loggerSet) settings
        handleColor = (\c -> if c then id else unAnnotate) $ _colorizeLogs settings
        refLevel = _logLevel settings
    when (l >= refLevel) $ lift $ pushLogStrLn loggerSet $ toLogStr $ renderLazy $ layoutPretty defaultLayoutOptions $ handleColor t

  -- getLogLevel :: m LogLevel
  getLogLevel = _logLevel <$> (readMVar =<< ask)

  -- setLogLevel :: LogLevel -> m ()
  setLogLevel level = do
    mvar <- ask
    modifyMVar_ mvar $ \settings -> return (settings { _logLevel = level })

  -- setColorizeLogs :: Bool -> m ()
  setColorizeLogs value = do
    mvar <- ask
    modifyMVar_ mvar $ \settings -> return (settings { _colorizeLogs = value })

  -- flushLogs :: m ()
  flushLogs = do
    settings <- readMVar =<< ask
    lift $ flushLogStr $ _loggerSet settings
    lift $ flushLogStr $ _errorLoggerSet settings
