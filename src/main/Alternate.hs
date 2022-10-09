{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Alternate (AlternateException (..), extractAlternateLinks) where

-- {{{ Imports
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import Imm.Link
import Imm.Logger as Logger
import Imm.Pretty
import Pipes
import Pipes.ByteString as Pipes (toHandle)
import System.IO (hClose)
import System.Process.Typed
import URI.ByteString.Extended

-- }}}

newtype AlternateException = FeedNotFound URI
  deriving (Eq, Show)

instance Exception AlternateException where
  displayException = show . pretty

instance Pretty AlternateException where
  pretty (FeedNotFound uri) = "No feeds found at" <+> prettyURI uri

data AlternateLink = AlternateLink Text Text Text
  deriving (Eq, Ord, Show)

instance Pretty AlternateLink where
  pretty (AlternateLink title linkType uri) = "Alternate link" <+> braces (hsep parameters)
    where
      parameters =
        [ "title" <> equals <> dquotes (pretty title),
          "type" <> equals <> dquotes (pretty linkType),
          "href" <> equals <> dquotes (pretty uri)
        ]

instance FromJSON AlternateLink where
  parseJSON = withObject "AlternateLink" $ \v ->
    AlternateLink
      <$> (v .: "title" <|> pure mempty)
      <*> v .: "type"
      <*> v .: "href"

asFeedURI :: MonadFail m => URI -> Text -> m URI
asFeedURI baseURI href =
  let bytes = encodeUtf8 @Text href
   in case parseURI laxURIParserOptions bytes of
        Right uri -> pure uri
        _ -> case parseRelativeRef laxURIParserOptions bytes of
          Right relativeRef -> pure $ mergeURIs baseURI relativeRef
          Left e -> fail $ show e

mergeURIs :: URI -> URIRef Relative -> URI
mergeURIs uri relativeRef = URI scheme authority path query fragment
  where
    scheme = uriScheme uri
    authority = rrAuthority relativeRef <|> uriAuthority uri
    path = mergePaths (uriPath uri) (rrPath relativeRef)
    query = rrQuery relativeRef
    fragment = rrFragment relativeRef

mergePaths :: ByteString -> ByteString -> ByteString
mergePaths basePath path = case Char8.head path of
  '/' -> path
  _ -> basePath <> "/" <> path

asLink :: MonadFail m => URI -> AlternateLink -> m Link
asLink baseURI (AlternateLink title type' uri) = Link (Just Alternate) title (parseMediaType type') . AnyURI <$> asFeedURI baseURI uri

extractAlternateLinks :: MonadIO m => MonadFail m => Logger.Handle IO -> URI -> Producer ByteString IO () -> m [Link]
extractAlternateLinks logger baseUri html = io $
  withProcessWait pup $ \pupProcess -> do
    log logger Debug $ pretty $ show @String pupProcess

    runEffect $ html >-> toHandle (getStdin pupProcess)
    hClose (getStdin pupProcess)

    links <- getStdout pupProcess & atomically <&> decode <&> fromMaybe mempty
    log logger Info $ "Found alternate links:" <+> pretty links

    mapM (asLink baseUri) links
  where
    pup =
      proc "pup" ["html head link[rel=\"alternate\"] json{}"]
        & setStdin createPipe
        & setStdout byteStringOutput
        & setStderr nullStream
