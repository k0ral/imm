{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Hooks module to define the main behavior of the program.
module Imm.Hooks where

-- {{{ Imports
import           Imm.Feed
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty
-- }}}

-- * Types

-- | Monad capable of acting on specific events.
class Monad m => MonadImm m where
  -- | Action triggered for each unread feed element
  processNewElement :: Feed -> FeedElement -> m ()

-- * Primitives

onNewElement :: (MonadImm m, MonadLog m) => Feed -> FeedElement -> m ()
onNewElement feed element = do
  logDebug $ "Unread element:" <+> pretty (getTitle element)
  processNewElement feed element
