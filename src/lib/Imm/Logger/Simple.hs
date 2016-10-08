{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simple logger interpreter.
-- For further information, please consult "System.Log.FastLogger".
module Imm.Logger.Simple (module Imm.Logger.Simple, module Reexport) where

-- {{{ Imports
import           Imm.Logger            as Reexport
import           Imm.Prelude
import           Imm.Pretty

import           System.Log.FastLogger as Reexport
-- }}}

-- * Settings

data LoggerSettings = LoggerSettings
  { loggerSet      :: LoggerSet  -- ^ 'LoggerSet' used for 'Debug', 'Info' and 'Warning' logs
  , errorLoggerSet :: LoggerSet  -- ^ 'LoggerSet' used for 'Error' logs
  , logLevel       :: LogLevel   -- ^ Discard logs that are strictly less serious than this level
  , colorizeLogs   :: Bool       -- ^ Enable log colorisation
  }

-- | Default logger forwards error messages to stderr, and other messages to stdout.
defaultLogger :: MonadIO m => m LoggerSettings
defaultLogger = io $ LoggerSettings
  <$> newStdoutLoggerSet defaultBufSize
  <*> newStderrLoggerSet defaultBufSize
  <*> pure Info
  <*> pure True

-- * Interpreter

-- | Interpreter for 'LoggerF'
mkCoLogger :: (MonadIO m) => LoggerSettings -> CoLoggerF m LoggerSettings
mkCoLogger settings = CoLoggerF coLog coGetLevel coSetLevel coSetColorize coFlush where
  coLog Error t = do
    io $ pushLogStrLn (errorLoggerSet settings) $ toLogStr $ (show :: Doc -> String) $ handleColor $ red t
    return settings
  coLog l t = do
    when (l >= logLevel settings) $ io $ pushLogStrLn (loggerSet settings) $ toLogStr $ (show :: Doc -> String) $ handleColor t
    return settings
  coGetLevel = return (logLevel settings, settings)
  coSetLevel l = return $ settings { logLevel = l }
  coSetColorize c = return $ settings { colorizeLogs = c }
  coFlush = do
    io $ flushLogStr $ loggerSet settings
    io $ flushLogStr $ errorLoggerSet settings
    return settings
  handleColor = if colorizeLogs settings then id else plain
