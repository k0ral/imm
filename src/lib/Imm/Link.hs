{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
-- | Simplified model for RFC 8288 links.
--
-- Cf <https://tools.ietf.org/html/rfc8288> . .
module Imm.Link
  ( Link(..)
  , Relation(..)
  , parseRelation
  , MediaType(..)
  , parseMediaType
  , pattern MediaTypeRSS
  , pattern MediaTypeAtom
  , pattern MediaTypeApplicationXML
  , pattern MediaTypeTextXML
  , pattern MediaTypeHTML
  ) where

import           Data.Aeson
import           Text.Parsec             (Stream, parse)
import           Text.Parser.Char
import           URI.ByteString.Extended


data Link = Link
  { _linkRelation :: Maybe Relation
  , _linkTitle    :: Text
  , _linkType     :: Maybe MediaType
  , _linkURI      :: AnyURI
  } deriving(Eq, Generic, Ord, Show, Typeable)

linkOptions :: Options
linkOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_link")
  , omitNothingFields = True
  }

instance ToJSON Link where
  toJSON     = genericToJSON linkOptions
  toEncoding = genericToEncoding linkOptions

instance FromJSON Link where
  parseJSON = genericParseJSON linkOptions


-- | Cf <https://www.iana.org/assignments/link-relations/link-relations.xhtml> .
data Relation = Alternate | Edit | Next | NoFollow | Replies | Self | OtherRelation Text
  deriving (Eq, Generic, Ord, Read, Show, Typeable)

instance ToJSON Relation where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Relation

parseRelation :: Text -> Maybe Relation
parseRelation "alternate" = pure Alternate
parseRelation "edit"      = pure Edit
parseRelation "next"      = pure Next
parseRelation "nofollow"  = pure NoFollow
parseRelation "replies"   = pure Replies
parseRelation "self"      = pure Self
parseRelation ""          = Nothing
parseRelation t           = pure $ OtherRelation t


-- | https://tools.ietf.org/html/rfc6838
-- https://en.wikipedia.org/wiki/Media_type
data MediaType = MediaType
  { _mediaType       :: Text
  , _mediaSubtype    :: Text
  , _mediaSuffix     :: Text
  , _mediaParameters :: [Text]
  } deriving (Eq, Generic, Ord, Read, Show, Typeable)

mediaTypeOptions :: Options
mediaTypeOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_media")
  , omitNothingFields = True
  }

instance ToJSON MediaType where
  toJSON     = genericToJSON mediaTypeOptions
  toEncoding = genericToEncoding mediaTypeOptions

instance FromJSON MediaType where
  parseJSON = genericParseJSON mediaTypeOptions


parseMediaType :: Stream s Identity Char => s -> Maybe MediaType
parseMediaType = either (const Nothing) pure . parse parser "" where
  parser = do
    t <- some $ noneOf "/"
    char '/'
    subtype <- some $ noneOf "+;"
    suffix <- optional $ char '+' >> some (noneOf ";")
    parameters <- many $ char ';' >> some (noneOf ";")

    return $ MediaType (toText t) (toText subtype) (toText $ fromMaybe mempty suffix) $ map toText parameters


pattern MediaTypeRSS :: [Text] -> MediaType
pattern MediaTypeRSS parameters = MediaType "application" "rss" "xml" parameters

pattern MediaTypeAtom :: [Text] -> MediaType
pattern MediaTypeAtom parameters = MediaType "application" "atom" "xml" parameters

pattern MediaTypeApplicationXML :: Text -> [Text] -> MediaType
pattern MediaTypeApplicationXML suffix parameters = MediaType "application" "xml" suffix parameters

pattern MediaTypeTextXML :: Text -> [Text] -> MediaType
pattern MediaTypeTextXML suffix parameters = MediaType "text" "xml" suffix parameters

pattern MediaTypeHTML :: Text -> [Text] -> MediaType
pattern MediaTypeHTML suffix parameters = MediaType "text" "html" suffix parameters
