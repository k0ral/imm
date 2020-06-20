{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Alternate (AlternateException(..), resolveFeedURI) where

-- {{{ Imports
import           Control.Exception.Safe
import           Data.Aeson
import qualified Data.ByteString.Char8  as Char8
import qualified Data.Text              as Text
import           Imm.Feed
import qualified Imm.HTTP               as HTTP
import           Imm.Logger             as Logger
import           Imm.Pretty
import           Pipes
import           Pipes.ByteString       as Pipes (toHandle)
import           Safe
import           System.IO              (hClose)
import           System.Process.Typed
import           URI.ByteString
-- }}}


newtype AlternateException = FeedNotFound URI
  deriving(Eq, Show)

instance Exception AlternateException where
  displayException = show . pretty

instance Pretty AlternateException where
  pretty (FeedNotFound uri) = "No feeds found at" <+> prettyURI uri


data AlternateLink = AlternateLink Text Text Text
  deriving(Eq, Ord, Show)

instance Pretty AlternateLink where
  pretty (AlternateLink title linkType uri) = "Alternate link" <+> braces (hsep parameters) where
    parameters = [ "title" <> equals <> dquotes (pretty title)
                 , "type" <> equals <> dquotes (pretty linkType)
                 , "href" <> equals <> dquotes (pretty uri)
                 ]

instance FromJSON AlternateLink where
  parseJSON (Object v) = AlternateLink <$> v .: "title" <*> v .: "type" <*> v .: "href"
  parseJSON _          = mzero


data FeedType = RssXml | AtomXml
  deriving(Eq, Ord, Read, Show)

asFeedType :: MonadFail m => Text -> m FeedType
asFeedType "application/rss+xml"  = pure RssXml
asFeedType "application/atom+xml" = pure AtomXml
asFeedType t                      = fail $ "Invalid feed type: " <> show t


asFeedURI :: MonadFail m => URI -> Text -> m URI
asFeedURI baseURI href = let bytes = encodeUtf8 @Text href in
  case parseURI laxURIParserOptions bytes of
    Right uri -> pure uri
    _ -> case parseRelativeRef laxURIParserOptions bytes of
      Right relativeRef -> pure $ mergeURIs baseURI relativeRef
      Left e            -> fail $ show e

mergeURIs :: URI -> URIRef Relative -> URI
mergeURIs uri relativeRef = URI scheme authority path query fragment where
  scheme = uriScheme uri
  authority = rrAuthority relativeRef <|> uriAuthority uri
  path = mergePaths (uriPath uri) (rrPath relativeRef)
  query = rrQuery relativeRef
  fragment = rrFragment relativeRef

mergePaths :: ByteString -> ByteString -> ByteString
mergePaths basePath path = case Char8.head path of
  '/' -> path
  _   -> basePath <> "/" <> path


data FeedLink = FeedLink
  { _feedTitle :: Text
  , _feedType  :: FeedType
  , _feedURI   :: URI
  } deriving(Eq, Ord, Show)

asFeedLink :: MonadFail m => URI -> AlternateLink -> m FeedLink
asFeedLink baseURI (AlternateLink a b c) = FeedLink a <$> asFeedType b <*> asFeedURI baseURI c


extractAlternateLinks :: MonadIO m => Logger.Handle IO -> Producer' ByteString IO () -> m [AlternateLink]
extractAlternateLinks logger html = io $ withProcessWait pup $ \pupProcess -> do
  log logger Debug $ pretty $ show @String pupProcess

  runEffect $ html >-> toHandle (getStdin pupProcess)
  hClose (getStdin pupProcess)

  links <- getStdout pupProcess & atomically <&> decode <&> fromMaybe mempty
  log logger Info $ "Found alternate links:" <+> pretty links

  return links
  where pup = proc "pup" ["html head link[rel=\"alternate\"] json{}"]
          & setStdin createPipe
          & setStdout byteStringOutput
          & setStderr nullStream


resolveFeedURI :: m ~ IO => Logger.Handle m -> HTTP.Handle m -> FeedLocation -> m URI
resolveFeedURI _ _ (FeedDirectURI uri) = pure uri
resolveFeedURI logger httpClient (FeedAlternateLink uri title) = HTTP.withGet logger httpClient uri $ \html -> extractAlternateLinks logger html
  <&> concatMap (asFeedLink uri)
  <&> filterByTitle
  <&> maximumByMay compareFeeds
  >>= maybe (throwM $ FeedNotFound uri) (return . _feedURI)
  where filterByTitle = if Text.null title then id else filter (\f -> _feedTitle f == title)
        compareFeeds (FeedLink _ AtomXml _) (FeedLink _ RssXml _) = GT
        compareFeeds (FeedLink _ RssXml _) (FeedLink _ AtomXml _) = LT
        compareFeeds _ _                                          = EQ
