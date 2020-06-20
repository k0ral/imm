{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | HTTP module abstracts over HTTP requests to the external world.
--
-- This module follows the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
--
-- > import qualified Imm.HTTP as HTTP
module Imm.HTTP where

-- {{{ Imports
import           Imm.Logger     hiding (Handle)
import qualified Imm.Logger     as Logger
import           Imm.Pretty

import           Pipes.Core
import           URI.ByteString
-- }}}

-- * Types

-- | Handle to perform GET HTTP requests.
newtype Handle m = Handle
  { _withGet :: forall a. URI -> (Producer' ByteString m () -> m a) -> m a
  }


-- * Primitives

-- | Simple wrapper around '_withGet' that also logs the requested URI.
withGet :: Monad m => Logger.Handle m -> Handle m -> URI -> (Producer' ByteString m () -> m a) -> m a
withGet logger handle uri f = do
  log logger Info $ "GET" <+> prettyURI uri
  _withGet handle uri f
