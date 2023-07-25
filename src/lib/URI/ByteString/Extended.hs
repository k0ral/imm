{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

module URI.ByteString.Extended (module URI.ByteString.Extended, module URI.ByteString) where

import Data.Aeson.Extended
import Data.Type.Equality
import Imm.Pretty
import URI.ByteString

data AnyURI = ∀ a. AnyURI (URIRef a)

deriving instance Show AnyURI
instance Eq AnyURI where
  (AnyURI a) == (AnyURI b) = case sameURIType a b of
    Just Refl → a == b
    _ → False

instance Ord AnyURI where
  compare (AnyURI a) (AnyURI b) = case (a, b) of
    (URI{}, URI{}) → compare a b
    (RelativeRef{}, RelativeRef{}) → compare a b
    (URI{}, RelativeRef{}) → LT
    (RelativeRef{}, URI{}) → GT

instance Pretty AnyURI where
  pretty (AnyURI a@URI{}) = prettyURI a
  pretty (AnyURI a@RelativeRef{}) = prettyURI a

instance ToJSON AnyURI where
  toJSON (AnyURI a@URI{}) = toJSON $ String $ decodeUtf8 $ serializeURIRef' a
  toJSON (AnyURI a@RelativeRef{}) = toJSON $ String $ decodeUtf8 $ serializeURIRef' a

instance FromJSON AnyURI where
  parseJSON = withText "URI" $ \s →
    let bytes = encodeUtf8 s
        uri = parseURI laxURIParserOptions bytes
        relativeRef = parseRelativeRef laxURIParserOptions bytes
     in either (const $ fail "Invalid URI") pure $ (AnyURI <$> uri) <> (AnyURI <$> relativeRef)

sameURIType ∷ URIRef a1 → URIRef a2 → Maybe (URIRef a1 :~: URIRef a2)
sameURIType a b = case (a, b) of
  (URI{}, URI{}) → Just Refl
  (RelativeRef{}, RelativeRef{}) → Just Refl
  _ → Nothing

withAnyURI ∷ (∀ a. URIRef a → b) → AnyURI → b
withAnyURI f (AnyURI a) = f a

toAbsoluteURI ∷ Scheme → AnyURI → URI
toAbsoluteURI scheme (AnyURI a) = toAbsolute scheme a
