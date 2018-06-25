{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of "Imm.Hooks" that does nothing,
-- except suggesting the user to define proper hooks.
--
-- This is the default implementation of the program.
module Imm.Hooks.Dummy where

-- {{{ Imports
import           Imm.Hooks
import           Imm.Prelude

import           Control.Exception
import           Control.Monad.Trans.Reader
-- }}}

data DummyHooks = DummyHooks

instance MonadImm (ReaderT DummyHooks IO) where
  processNewElement _ _ = throwM $ NoMethodError "Please define a valid Imm.Hooks.processNewElement function"
