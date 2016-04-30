{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Imm.Aeson where

-- {{{ Imports
import           Imm.Prelude

import           Data.Aeson

import           URI.ByteString
-- }}}

parseJsonURI :: (MonadPlus m) => Value -> m URI
parseJsonURI (String s) = either (const mzero) return $ parseURI laxURIParserOptions $ encodeUtf8 s
parseJsonURI _          = mzero

toJsonURI :: URI -> Value
toJsonURI = String . decodeUtf8 . serializeURIRef'
