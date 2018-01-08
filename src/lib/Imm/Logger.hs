{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
-- | DSL/interpreter model for the logger
module Imm.Logger where

-- {{{ Imports
import           Imm.Prelude

import           Control.Monad.Trans.Free
-- }}}

-- * Types

data LogLevel = Debug | Info | Warning | Error
  deriving(Eq, Ord, Read, Show)

instance Pretty LogLevel where
  pretty Debug   = text "DEBUG"
  pretty Info    = text "INFO"
  pretty Warning = text "WARNING"
  pretty Error   = text "ERROR"

-- | Logger DSL
data LoggerF next
  = Log LogLevel Doc next
  | GetLevel (LogLevel -> next)
  | SetLevel LogLevel next
  | SetColorize Bool next
  | Flush next
  deriving(Functor)

-- | Logger interpreter
data CoLoggerF m a = CoLoggerF
  { logH         :: LogLevel -> Doc -> m a
  , getLevelH    :: m (LogLevel, a)
  , setLevelH    :: LogLevel -> m a
  , setColorizeH :: Bool -> m a
  , flushH       :: m a
  } deriving(Functor)

instance Monad m => PairingM (CoLoggerF m) LoggerF m where
  -- pairM :: (a -> b -> m r) -> f a -> g b -> m r
  pairM p CoLoggerF{logH} (Log level message next) = do
    a <- logH level message
    p a next
  pairM p CoLoggerF{getLevelH} (GetLevel next) = do
    (l, a) <- getLevelH
    p a (next l)
  pairM p CoLoggerF{setLevelH} (SetLevel level next) = do
    a <- setLevelH level
    p a next
  pairM p CoLoggerF{setColorizeH} (SetColorize colorize next) = do
    a <- setColorizeH colorize
    p a next
  pairM p CoLoggerF{flushH} (Flush next) = do
    a <- flushH
    p a next

-- * Primitives

log :: (MonadFree (SumF f) m, LoggerF :<: f) => LogLevel -> Doc -> m ()
log level message = liftF . inj $ Log level message ()

getLogLevel :: (MonadFree (SumF f) m, LoggerF :<: f) => m LogLevel
getLogLevel = liftF . inj $ GetLevel id

setLogLevel :: (MonadFree (SumF f) m, LoggerF :<: f) => LogLevel -> m ()
setLogLevel level = liftF . inj $ SetLevel level ()

setColorizeLogs :: (MonadFree (SumF f) m, LoggerF :<: f) => Bool -> m ()
setColorizeLogs colorize = liftF . inj $ SetColorize colorize ()

flushLogs :: (MonadFree (SumF f) m, LoggerF :<: f) => m ()
flushLogs = liftF . inj $ Flush ()

-- * Helpers

logDebug, logInfo, logWarning, logError :: (MonadFree (SumF f) m, LoggerF :<: f) => Doc -> m ()
logDebug = log Debug
logInfo = log Info
logWarning = log Warning
logError = log Error
