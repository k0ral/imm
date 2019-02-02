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
import           Control.Monad.Trans.Control
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


mkHandle :: MonadBaseControl IO m => MVar LoggerSettings -> Handle m
mkHandle settings = Handle
  { log = \l t -> do
      s <- readMVar settings
      let loggerSet = (if l == Error then _errorLoggerSet else _loggerSet) s
          handleColor = (\c -> if c then id else unAnnotate) $ _colorizeLogs s
          refLevel = _logLevel s
      when (l >= refLevel) $ liftBase $ pushLogStrLn loggerSet $ toLogStr $ renderLazy $ layoutPretty defaultLayoutOptions $ handleColor t

  , getLogLevel = _logLevel <$> readMVar settings
  , setLogLevel = \level -> modifyMVar_ settings $ \s -> return (s { _logLevel = level })
  , setColorizeLogs = \value -> modifyMVar_ settings $ \s -> return (s { _colorizeLogs = value })
  , flushLogs = do
      s <- readMVar settings
      liftBase $ flushLogStr $ _loggerSet s
      liftBase $ flushLogStr $ _errorLoggerSet s
  }
