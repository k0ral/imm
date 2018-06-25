{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | HTTP module abstracts over HTTP requests to the external world.
module Imm.HTTP where

-- {{{ Imports
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty

import           URI.ByteString
-- }}}

-- * Types

-- | Monad capable of performing GET HTTP requests.
class MonadThrow m => MonadHttpClient m where
  httpGet :: URI -> m LByteString

-- * Primitives

-- | Simple wrapper around 'httpGet' that also logs the requested URI.
get :: (MonadHttpClient m, MonadLog m, MonadThrow m)
    => URI -> m LByteString
get uri = do
  logDebug $ "Fetching " <> prettyURI uri
  httpGet uri
