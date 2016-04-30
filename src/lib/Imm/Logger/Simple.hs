{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simple logger interpreter.
-- For further information, please consult "System.Log.FastLogger".
module Imm.Logger.Simple (module Imm.Logger.Simple, module Reexport) where

-- {{{ Imports
import           Imm.Logger            as Reexport
import           Imm.Prelude

import           System.Log.FastLogger as Reexport
-- }}}

-- * Settings

data LoggerSettings = LoggerSettings
  { loggerSet      :: LoggerSet  -- ^ 'LoggerSet' used for 'Debug', 'Info' and 'Warning' logs
  , errorLoggerSet :: LoggerSet  -- ^ 'LoggerSet' used for 'Error' logs
  , logLevel       :: LogLevel   -- ^ Discard logs that are strictly less serious than this level
  }

-- | Default logger forwards error messages to stderr, and other messages to stdout.
defaultLogger :: IO LoggerSettings
defaultLogger = LoggerSettings
  <$> newStdoutLoggerSet defaultBufSize
  <*> newStderrLoggerSet defaultBufSize
  <*> pure Info

-- * Interpreter

-- | Interpreter for 'LoggerF'
mkCoLogger :: (MonadIO m) => LoggerSettings -> CoLoggerF m LoggerSettings
mkCoLogger settings = CoLoggerF coLog coGetLevel coSetLevel where
  coLog Error t = do
    io $ pushLogStrLn (errorLoggerSet settings) $ toLogStr $ "ERROR: " <> t
    return settings
  coLog l t = do
    when (l >= logLevel settings) $ io $ pushLogStrLn (loggerSet settings) $ toLogStr $ show (pretty l) <> ": " <> t
    return settings
  coGetLevel = return (logLevel settings, settings)
  coSetLevel l = return $ settings { logLevel = l }
