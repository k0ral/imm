{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
-- | DSL/interpreter model for the logger
module Imm.Logger where

-- {{{ Imports
import           Imm.Prelude

import           Control.Monad.Trans.Free

-- import           Text.PrettyPrint.ANSI.Leijen
-- }}}

-- * Types

data LogLevel = Debug | Info | Warning | Error
  deriving(Eq, Ord, Read, Show)

instance Pretty LogLevel where
  pretty Debug = text "DEBUG"
  pretty Info = text "INFO"
  pretty Warning = text "WARNING"
  pretty Error = text "ERROR"

-- | Logger DSL
data LoggerF next
  = Log LogLevel Text next
  | GetLevel (LogLevel -> next)
  | SetLevel LogLevel next
  deriving(Functor)

-- | Logger interpreter
data CoLoggerF m a = CoLoggerF
  { logH      :: LogLevel -> Text -> m a
  , getLevelH :: m (LogLevel, a)
  , setLevelH :: LogLevel -> m a
  } deriving(Functor)

instance Monad m => PairingM (CoLoggerF m) LoggerF m where
  -- pairM :: (a -> b -> m r) -> f a -> g b -> m r
  pairM p (CoLoggerF l _ _) (Log level message next) = do
    a <- l level message
    p a next
  pairM p (CoLoggerF _ gl _) (GetLevel next) = do
    (l, a) <- gl
    p a (next l)
  pairM p (CoLoggerF _ _ sl) (SetLevel level next) = do
    a <- sl level
    p a next

-- * Primitives

log :: (Functor f, MonadFree f m, LoggerF :<: f) => LogLevel -> Text -> m ()
log level message = liftF . inj $ Log level message ()

getLogLevel :: (Functor f, MonadFree f m, LoggerF :<: f) => m LogLevel
getLogLevel = liftF . inj $ GetLevel id

setLogLevel :: (Functor f, MonadFree f m, LoggerF :<: f) => LogLevel -> m ()
setLogLevel level = liftF . inj $ SetLevel level ()

-- * Helpers

logDebug, logInfo, logWarning, logError :: (Functor f, MonadFree f m, LoggerF :<: f) => Text -> m ()
logDebug = log Debug
logInfo = log Info
logWarning = log Warning
logError = log Error
