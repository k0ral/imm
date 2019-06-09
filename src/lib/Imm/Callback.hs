{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Imm.Callback where

-- {{{ Imports
import           Imm.Feed

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.Prettyprint.Doc
import           Dhall
-- }}}

-- | External program run for each feed element.
--
-- A `Message` is passed to this program through stdin, serialized in JSON.
data Callback = Callback
  { _executable :: FilePath
  , _arguments  :: [Text]
  } deriving (Eq, Generic, Ord, Read, Show)

instance Interpret Callback

instance Pretty Callback where
  pretty (Callback executable arguments) = pretty executable <+> sep (pretty <$> arguments)


-- | All information passed to external programs about a new feed item, are stored in this structure.
data Message = Message Feed FeedElement deriving(Eq, Generic, Ord, Show)

instance ToJSON Message where
  toJSON (Message feed element) = object ["feed" .= renderFeed feed, "element" .= renderFeedElement element]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> (v .: "feed" >>= (liftEither . parseFeed))
    <*> (v .: "element" >>= (liftEither . parseFeedElement))
    where liftEither :: Either e a -> Parser a
          liftEither = either (const mempty) return
