{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
-- | DSL/interpreter model for the HTTP client
module Imm.HTTP where

-- {{{ Imports
import           Imm.Error
import           Imm.Logger
import           Imm.Prelude
import           Imm.Pretty

import           Control.Monad.Trans.Free

import           URI.ByteString
-- }}}

-- * Types

-- | HTTP client DSL
data HttpClientF next
  = Get URI (Either SomeException LByteString -> next)
  deriving(Functor)

-- | HTTP client interpreter
newtype CoHttpClientF m a = CoHttpClientF
  { getH :: URI -> m (Either SomeException LByteString, a)
  } deriving(Functor)

instance Monad m => PairingM (CoHttpClientF m) HttpClientF m where
  -- pairM :: (a -> b -> m r) -> f a -> g b -> m r
  pairM p (CoHttpClientF g) (Get uri next) = do
    (result, a) <- g uri
    p a $ next result

-- * Primitives

-- | Perform an HTTP GET request
get :: (MonadFree f m, HttpClientF :<: f, LoggerF :<: f, MonadThrow m)
    => URI -> m LByteString
get uri = do
  logDebug $ "Fetching " <> prettyURI uri
  result <- liftF . inj $ Get uri id
  liftE result
