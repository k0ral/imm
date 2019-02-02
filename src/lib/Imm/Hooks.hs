{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Hooks module abstracts over the main behavior of the program.
--
-- This module follows the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
--
-- > import qualified Imm.Hooks as Hooks
module Imm.Hooks where

-- {{{ Imports
import           Imm.Feed
import qualified Imm.Logger as Logger
import           Imm.Logger hiding(Handle)
import           Imm.Prelude
import           Imm.Pretty
-- }}}

-- * Types

newtype Handle m = Handle
  { processNewElement :: Feed -> FeedElement -> m ()  -- ^ Action triggered for each unread feed element
  }

-- * Primitives

onNewElement :: Monad m => Logger.Handle m -> Handle m -> Feed -> FeedElement -> m ()
onNewElement logger handle feed element = do
  log logger Debug $ "Unread element:" <+> pretty (getTitle element)
  processNewElement handle feed element
