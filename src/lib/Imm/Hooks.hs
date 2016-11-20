{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
-- | DSL/interpreter model for hooks, ie various events that can trigger arbitrary actions
module Imm.Hooks where

-- {{{ Imports
import           Imm.Feed
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty

import           Control.Monad.Free.Class
-- }}}

-- * Types

-- | Hooks DSL
data HooksF next
  = OnNewElement Feed FeedElement next
  deriving(Functor)

-- | Hooks interpreter
data CoHooksF m a = CoHooksF
  { onNewElementH :: Feed -> FeedElement -> m a  -- ^ Triggered for each unread feed element
  } deriving(Functor)

instance Monad m => PairingM (CoHooksF m) HooksF m where
  -- pairM :: (a -> b -> m r) -> f a -> g b -> m r
  pairM p (CoHooksF f) (OnNewElement feed element next) = do
    a <- f feed element
    p a next

-- * Primitives

onNewElement :: (MonadFree f m, LoggerF :<: f, HooksF :<: f) => Feed -> FeedElement -> m ()
onNewElement feed element = do
  logDebug $ "Unread element:" <+> textual (getTitle element)
  liftF . inj $ OnNewElement feed element ()
