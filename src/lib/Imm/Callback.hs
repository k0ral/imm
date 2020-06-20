{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Imm.Callback (Callback(..), serializeMessage, deserializeMessage) where

-- {{{ Imports
import           Imm.Feed

import qualified Data.Avro                 as Avro
import           Data.Avro.Deriving
import           Data.Text.Prettyprint.Doc
import           Dhall                     hiding (maybe)
-- }}}

-- | External program run for each feed element.
--
-- Data is passed to that program through standard input (@stdin@), using Avro (<https://hackage.haskell.org/package/avro>) serialization format. The data schema is described in file @idl/callback.json@, provided with this library.
data Callback = Callback
  { _executable :: FilePath
  , _arguments  :: [Text]
  } deriving (Eq, Generic, Ord, Read, Show)

instance FromDhall Callback

instance Pretty Callback where
  pretty (Callback executable arguments) = pretty executable <+> sep (pretty <$> arguments)


deriveAvroWithOptions defaultDeriveOptions "idl/callback.json"

-- | Meant to be called by the main @imm@ process.
serializeMessage :: Feed -> FeedElement -> LByteString
serializeMessage feed element = Avro.encodeValue $ Message (renderFeed feed) (renderFeedElement element)

-- | Meant to be called by callback process.
deserializeMessage :: MonadFail m => LByteString -> m (Feed, FeedElement)
deserializeMessage bytestring = do
  Message feedText elementText <- Avro.decodeValue bytestring & either fail pure
  feed <- parseFeed feedText & either (fail . displayException) pure
  element <- parseFeedElement elementText & either (fail . displayException) pure
  return (feed, element)
