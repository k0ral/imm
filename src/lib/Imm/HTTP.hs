{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | HTTP module abstracts over HTTP requests to the external world.
--
-- This module follows the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
--
-- > import qualified Imm.HTTP as HTTP
module Imm.HTTP where

-- {{{ Imports
import qualified Imm.Logger as Logger
import           Imm.Logger hiding(Handle)
import           Imm.Prelude
import           Imm.Pretty

import           URI.ByteString
-- }}}

-- * Types

-- | Handle to perform GET HTTP requests.
newtype Handle m = Handle
  { httpGet :: URI -> m LByteString
  }


-- * Primitives

-- | Simple wrapper around 'httpGet' that also logs the requested URI.
get :: Monad m => Logger.Handle m -> Handle m -> URI -> m LByteString
get logger handle uri = do
  log logger Debug $ "Fetching " <> prettyURI uri
  httpGet handle uri
