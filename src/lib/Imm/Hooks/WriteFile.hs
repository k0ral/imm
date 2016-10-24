{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
-- | Hooks interpreter that writes a file for each element.
module Imm.Hooks.WriteFile where

-- {{{ Imports
import           Imm.Feed
import           Imm.Hooks
import           Imm.Prelude
import           Imm.Pretty

import           Data.Monoid.Textual           hiding (map)
import qualified Data.Text.Lazy                as Text
import           Data.Time

import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath

import           Text.Atom.Types
import qualified Text.Blaze                    as Blaze
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              as H hiding (map)
import           Text.Blaze.Html5.Attributes   as H (charset, href)
import           Text.RSS.Types

import           URI.ByteString
-- }}}

-- * Settings

-- | Where and what to write in a file
data FileInfo = FileInfo FilePath ByteString

data WriteFileSettings = WriteFileSettings (Feed -> FeedElement -> FileInfo)

-- * Interpreter

-- | Interpreter for 'HooksF'
mkCoHooks :: (MonadIO m) => WriteFileSettings -> CoHooksF m WriteFileSettings
mkCoHooks a@(WriteFileSettings f) = CoHooksF coOnNewElement where
  coOnNewElement feed element = do
    let FileInfo path content = f feed element
    io $ createDirectoryIfMissing True $ takeDirectory path
    writeFile path content
    return a

-- | Wrapper around 'defaultFilePath' and 'defaultFileContent'
defaultSettings :: FilePath            -- ^ Root directory for 'defaultFilePath'
                -> WriteFileSettings
defaultSettings root = WriteFileSettings $ \feed element -> FileInfo
  (defaultFilePath root feed element)
  (defaultFileContent feed element)

-- | Generate a path @<root>/<feed title>/<element date>-<element title>.html@, where @<root>@ is the first argument
defaultFilePath :: FilePath -> Feed -> FeedElement -> FilePath
defaultFilePath root feed element = makeValid $ root </> feedTitle </> fileName <.> "html" where
  date = maybe "" (formatTime defaultTimeLocale "%F-") $ getDate element
  fileName = date <> convertText (sanitizePath $ getTitle element)
  feedTitle = convertText $ sanitizePath $ getFeedTitle feed
  sanitizePath = intercalate "-" . split isPathSeparator

-- | Generate an HTML page, with a title, a header and an article that contains the feed element
defaultFileContent :: Feed -> FeedElement -> ByteString
defaultFileContent feed element = encodeUtf8 $ Text.toStrict $ renderHtml $ docTypeHtml $ do
  H.head $ do
    H.meta ! H.charset "utf-8"
    H.title $ convertText $ getFeedTitle feed <> " | " <> getTitle element
  body $ do
    H.h1 $ convertText $ getFeedTitle feed
    article $ do
      defaultHeader feed element
      defaultBody feed element


-- * Low-level helpers

-- | Generate an HTML @<header>@ for a given feed element
defaultHeader :: Feed -> FeedElement -> Html
defaultHeader _ element@(RssElement item) = header $ do
  H.h2 $ maybe id (\uri -> H.a ! H.href uri) link $ convertText $ getTitle element
  unless (null author) $ address $ "Published by " >> convertText author
  forM_ (itemPubDate item) $ \date -> p $ " on " >> time (convertDoc $ prettyTime date)
  where link = withRssURI (convertDoc . prettyURI) <$> itemLink item
        author = itemAuthor item
defaultHeader _ element@(AtomElement entry) = header $ do
  H.h2 $ convertText $ getTitle element
  address $ do
    "Published by "
    forM_ (entryAuthors entry) $ \author -> do
      convertDoc $ prettyPerson author
      ", "
  p $ "on " >> time (convertDoc $ prettyTime $ entryUpdated entry)

-- | Generate the HTML content for a given feed element
defaultBody :: Feed -> FeedElement -> Html
defaultBody _ (RssElement item) = p $ preEscapedToHtml $ itemDescription item
defaultBody _ (AtomElement entry) = do
  unless (null links) $ p $ do
    "Related links:"
    H.ul $ forM_ links $ \uri -> H.li (H.a ! H.href (convertAtomURI uri) $ convertAtomURI uri)
  p $ preEscapedToHtml $ fromMaybe "<empty>" $ content <|> summary
  where links   = map linkHref $ entryLinks entry
        content = show . prettyAtomContent <$> entryContent entry :: Maybe Text
        summary = show . prettyAtomText <$> entrySummary entry :: Maybe Text


convertAtomURI :: (IsString t) => AtomURI -> t
convertAtomURI = withAtomURI convertURI

convertURI :: (IsString t) => URIRef a -> t
convertURI = convertText . decodeUtf8 . serializeURIRef'

convertText :: (IsString t) => Text -> t
convertText = fromString . toString (const "?")

convertDoc :: (IsString t) => Doc -> t
convertDoc = show
