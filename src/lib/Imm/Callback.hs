{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Imm.Callback where

-- {{{ Imports
import           Imm.Feed

import qualified Data.Map                  as Map
import           Data.MessagePack.Object
import           Data.Text.Prettyprint.Doc
import           Dhall                     hiding (maybe)
-- }}}

-- | External program run for each feed element.
--
-- A `Message` is passed to this program through stdin, serialized in JSON.
data Callback = Callback
  { _executable :: FilePath
  , _arguments  :: [Text]
  } deriving (Eq, Generic, Ord, Read, Show)

instance FromDhall Callback

instance Pretty Callback where
  pretty (Callback executable arguments) = pretty executable <+> sep (pretty <$> arguments)


-- | All information passed to external programs about a new feed item, are stored in this structure.
data Message = Message Feed FeedElement deriving(Eq, Generic, Ord, Show)

instance MessagePack Message where
  toObject (Message feed element) = toObject @(Map Text Text)
    $ Map.insert "feed" (renderFeed feed)
    $ Map.insert "element" (renderFeedElement element) mempty

  fromObject object = fromObject object >>= \m -> Message
    <$> (lookup @(Map Text Text) "feed" m >>= eitherToMaybe . parseFeed)
    <*> (lookup @(Map Text Text) "element" m >>= eitherToMaybe . parseFeedElement)
    where eitherToMaybe (Right a) = Just a
          eitherToMaybe _         = Nothing
