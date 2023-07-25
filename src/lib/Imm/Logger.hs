{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Logger module abstracts over logging data.
--
-- This module follows the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
--
-- > import qualified Imm.Logger as Logger (Handle)
-- > import Imm.Logger hiding (Handle)
module Imm.Logger where

-- {{{ Imports
import Imm.Pretty

-- }}}

-- * Types

data Handle m = Handle
  { log ∷ LogLevel → Doc AnsiStyle → m ()
  , getLogLevel ∷ m LogLevel
  , setLogLevel ∷ LogLevel → m ()
  }

data LogLevel = Debug | Info | Warning | Error
  deriving (Eq, Ord, Read, Show)

instance Pretty LogLevel where
  pretty Debug = "DEBUG"
  pretty Info = "INFO"
  pretty Warning = "WARNING"
  pretty Error = "ERROR"
