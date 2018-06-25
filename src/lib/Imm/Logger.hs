{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Logger module.
module Imm.Logger where

-- {{{ Imports
import           Imm.Prelude
import           Imm.Pretty
-- }}}

-- * Types

data LogLevel = Debug | Info | Warning | Error
  deriving(Eq, Ord, Read, Show)

instance Pretty LogLevel where
  pretty Debug   = "DEBUG"
  pretty Info    = "INFO"
  pretty Warning = "WARNING"
  pretty Error   = "ERROR"

-- | Monad capable of logging pretty text.
class Monad m => MonadLog m where
  log :: LogLevel -> Doc AnsiStyle -> m ()
  getLogLevel :: m LogLevel
  setLogLevel :: LogLevel -> m ()
  setColorizeLogs :: Bool -> m ()
  flushLogs :: m ()

-- * Helpers

logDebug, logInfo, logWarning, logError :: MonadLog m => Doc AnsiStyle -> m ()
logDebug = log Debug
logInfo = log Info
logWarning = log Warning
logError = log Error
