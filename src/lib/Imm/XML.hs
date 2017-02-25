{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeOperators         #-}
-- | DSL/interpreter model for parsing XML into a 'Feed'
module Imm.XML where

-- {{{ Imports
import           Imm.Error
import           Imm.Feed
import           Imm.Prelude

import           Control.Monad.Trans.Free

import           URI.ByteString
-- }}}

-- * Types

-- | XML parsing DSL
data XmlParserF next
  = ParseXml URI LByteString (Either SomeException Feed -> next)
  deriving(Functor)

-- | XML parsing interpreter
newtype CoXmlParserF m a = CoXmlParserF
  { parseXmlH :: URI -> LByteString -> m (Either SomeException Feed, a)
  } deriving(Functor)

instance Monad m => PairingM (CoXmlParserF m) XmlParserF m where
  -- pairM :: (a -> b -> m r) -> f a -> g b -> m r
  pairM f (CoXmlParserF p) (ParseXml uri bytestring next) = do
    (result, a) <- p uri bytestring
    f a $ next result

-- * Primitives

-- | Parse XML into a 'Feed'
parseXml :: (MonadFree f m, XmlParserF :<: f, MonadThrow m)
         => URI -> LByteString -> m Feed
parseXml uri bytestring = do
  result <- liftF . inj $ ParseXml uri bytestring id
  liftE result
