{-# LANGUAGE UnicodeSyntax #-}

module Data.Aeson.Extended (
  module Data.Aeson,
  module Data.Aeson.Types,
  module Data.Aeson.Extended,
)
where

import Data.Aeson
import Data.Aeson.Types
import URI.ByteString

-- | Newtype wrapper to provide 'FromJSON' and 'ToJSON' instances for 'URI'
newtype JsonURI = JsonURI {_unwrapURI ∷ URI}

instance FromJSON JsonURI where
  parseJSON = withText "URI" $ \s →
    either (const $ fail "Invalid URI") (return . JsonURI) $ parseURI laxURIParserOptions $ encodeUtf8 s

instance ToJSON JsonURI where
  toJSON = String . decodeUtf8 . serializeURIRef' . _unwrapURI
