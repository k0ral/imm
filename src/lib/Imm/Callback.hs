{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Imm.Callback (Callback(..), CallbackMessage(..), runCallback) where

-- {{{ Imports
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Text.Prettyprint.Doc
import           Dhall                     hiding (maybe)
import           Imm.Feed
import           Imm.Logger                as Logger
import           Imm.Pretty
import           System.Exit
import           System.Process.Typed
-- }}}

-- | External program run for each feed element.
--
-- Data is passed to that program through standard input (@stdin@).
data Callback = Callback
  { _executable :: FilePath
  , _arguments  :: [Text]
  } deriving (Eq, Generic, Ord, Read, Show)

instance FromDhall Callback

instance Pretty Callback where
  pretty (Callback executable arguments) = pretty executable <+> sep (pretty <$> arguments)

-- | Data structure passed to the external program, through JSON format.
--
-- The data schema is described in file @schema/imm.json@, provided with this library.
data CallbackMessage = CallbackMessage
  { _callbackFeedLocation   :: FeedLocation
  , _callbackFeedDefinition :: FeedDefinition
  , _callbackFeedItem       :: FeedItem
  } deriving(Eq, Generic, Ord, Show, Typeable)

customOptions :: Options
customOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length @[] "_callback")
  , omitNothingFields = True
  }

instance ToJSON CallbackMessage where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON CallbackMessage where
  parseJSON = genericParseJSON customOptions

instance Pretty (PrettyShort CallbackMessage) where
  pretty (PrettyShort (CallbackMessage location feed item)) = pretty location <+> "/" <+> prettyName feed <+> "/" <+> pretty (_itemTitle item)


runCallback :: MonadIO m
  => Logger.Handle m -> Callback -> CallbackMessage -> m (Either (Callback, Int, LByteString, LByteString) (Callback, LByteString, LByteString))
runCallback logger callback@(Callback executable arguments) message = do
  log logger Info $ "Running" <+> cyan (pretty executable) <+> "on" <+> prettyShort message
  log logger Debug $ "Callback message:" <+> pretty (decodeUtf8 @Text $ encodePretty message)

  let processInput = byteStringInput $ encode message
      processConfig = proc executable (toString <$> arguments) & setStdin processInput
  (exitCode, output, errors) <- readProcess processConfig

  case exitCode of
    ExitSuccess   -> return $ Right (callback, output, errors)
    ExitFailure i -> return $ Left (callback, i, output, errors)
