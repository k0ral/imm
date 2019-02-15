{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of "Imm.Hooks" that does nothing,
-- except suggesting the user to define proper hooks.
--
-- This is the default implementation of the program.
module Imm.Hooks.Dummy (module Imm.Hooks.Dummy, module Imm.Hooks) where

-- {{{ Imports
import           Imm.Hooks

import           Control.Exception
import           Control.Exception.Safe
-- }}}

data DummyHooks = DummyHooks

mkHandle :: MonadThrow m => Handle m
mkHandle = Handle
  { processNewElement = \_ _ -> throwM $ NoMethodError "Please define a valid Imm.Hooks.processNewElement function"
  }
