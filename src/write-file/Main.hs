{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Write a file from the input RSS/Atom item.
--
-- Meant to be use as a callback for imm.
-- {{{ Imports
import           Imm.Callback
import           Imm.Feed
import           Imm.Link
import           Imm.Pretty

import           Data.Aeson
import           Data.ByteString.Builder
import           Data.ByteString.Lazy          (getContents, writeFile)
import qualified Data.Text                     as Text (null, replace, unpack)
import           Data.Time
import           Options.Applicative
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5              (Html, docTypeHtml, preEscapedToHtml, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as H (charset, href)
import           URI.ByteString.Extended
-- }}}

data CliOptions = CliOptions
  { _directory :: FilePath
  , _dryRun    :: Bool
  } deriving (Eq, Ord, Read, Show)

parseOptions :: MonadIO m => m CliOptions
parseOptions = io $ execParser $ info (cliOptions <**> helper) $ progDesc description where
  description = "Write a file for each new RSS/Atom item. An intermediate folder will be created for each feed."

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> strOption (long "directory" <> short 'd' <> metavar "PATH" <> help "Root directory where files will be created.")
  <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")



main :: IO ()
main = do
  CliOptions directory dryRun <- parseOptions
  input <- getContents <&> eitherDecode

  case input :: Either String CallbackMessage of
    Right (CallbackMessage feedLocation feedDefinition item) -> do
      let content = defaultFileContent feedDefinition item
          filePath = defaultFilePath directory feedLocation feedDefinition item
      putStrLn filePath
      unless dryRun $ do
        createDirectoryIfMissing True $ takeDirectory filePath
        writeFile filePath $ toLazyByteString content
    Left e -> putStrLn ("Invalid input: " <> e) >> exitFailure
  return ()

-- * Default behavior

-- | Generate a path @<root>/<feed designator>/<element date>-<element title>.html@, where @<root>@ is the first argument
defaultFilePath :: FilePath -> FeedLocation -> FeedDefinition -> FeedItem -> FilePath
defaultFilePath root feedLocation feedDefinition element = makeValid $ root </> feedFolder </> fileName <.> "html" where
  FeedLocation feedUri _ = feedLocation
  feedFolder = if Text.null title then uriToFolder feedUri else toString title
  uriToFolder uri = uri & uriAuthority <&> authorityHost <&> hostBS
    <&> decodeUtf8 & fromMaybe "unknown-host" & sanitize & Text.unpack
  date = maybe "" (formatTime defaultTimeLocale "%F-") $ _itemDate element
  fileName = date <> toString (sanitize $ _itemTitle element)
  title = sanitize $ _feedTitle feedDefinition
  sanitize = appEndo (mconcat [Endo $ Text.replace (toText [s]) "_" | s <- pathSeparators])
    >>> Text.replace "." "_"
    >>> Text.replace "?" "_"
    >>> Text.replace "!" "_"
    >>> Text.replace "#" "_"

-- | Generate an HTML page, with a title, a header and an article that contains the feed element
defaultFileContent :: FeedDefinition -> FeedItem -> Builder
defaultFileContent feedDefinition element = renderHtmlBuilder $ docTypeHtml $ do
  H.head $ do
    H.meta ! H.charset "utf-8"
    H.title $ convertText $ _feedTitle feedDefinition <> " | " <> _itemTitle element
  H.body $ do
    H.h1 $ convertText $ _feedTitle feedDefinition
    H.article $ do
      H.header $ do
        defaultArticleTitle feedDefinition element
        defaultArticleAuthor feedDefinition element
        defaultArticleDate feedDefinition element
      defaultBody feedDefinition element


-- * Low-level helpers

defaultArticleTitle :: FeedDefinition -> FeedItem -> Html
defaultArticleTitle _ item = H.h2
  $ maybe id (\link -> H.a ! href (_linkURI link)) (getMainLink item)
  $ convertText $ _itemTitle item

defaultArticleAuthor :: FeedDefinition -> FeedItem -> Html
defaultArticleAuthor _ item = H.address $ do
  "Published by "
  forM_ (_itemAuthors item) $ \author -> do
    convertDoc $ pretty author
    ", "

defaultArticleDate :: FeedDefinition -> FeedItem -> Html
defaultArticleDate _ element = forM_ (_itemDate element) $ \date -> H.p $ " on " >> H.time (convertDoc $ prettyTime date)


-- | Generate the HTML content for a given feed element
defaultBody :: FeedDefinition -> FeedItem -> Html
defaultBody _ item = do
  unless (null links) $ H.p $ do
    "Related links:"
    H.ul $ forM_ links $ \uri -> H.li (H.a ! href uri $ convertURI uri)
  H.p $ preEscapedToHtml $ _itemContent item
  where links   = _linkURI <$> _itemLinks item

href :: AnyURI -> H.Attribute
href = H.href . convertURI

convertURI :: (IsString t) => AnyURI -> t
convertURI = convertText . decodeUtf8 . withAnyURI serializeURIRef'

convertText :: (IsString t) => Text -> t
convertText = fromString . toString

convertDoc :: (IsString t) => Doc a -> t
convertDoc = show
