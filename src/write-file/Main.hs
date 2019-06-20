{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Write a file from the input RSS/Atom item.
--
-- Meant to be use as a callback for imm.
-- {{{ Imports
import           Imm.Callback
import           Imm.Feed
import           Imm.Pretty

import           Data.ByteString               (getContents)
import           Data.ByteString.Builder
import           Data.ByteString.Streaming     (toStreamingByteString)
import qualified Data.MessagePack              as MsgPack
import qualified Data.Text                     as Text (null, replace)
import           Data.Time
import           Options.Applicative
import           Streaming.With
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath
import           Text.Atom.Types
import           Text.Blaze.Html.Renderer.Utf8
import           Text.Blaze.Html5              (Html, docTypeHtml,
                                                preEscapedToHtml, (!))
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as H (charset, href)
import           Text.RSS.Types
import           URI.ByteString
-- }}}

data CliOptions = CliOptions
  { _directory :: FilePath
  , _dryRun    :: Bool
  } deriving (Eq, Ord, Read, Show)

parseOptions :: MonadIO m => m CliOptions
parseOptions = io $ customExecParser defaultPrefs (info cliOptions $ progDesc "Write a file for each new RSS/Atom item. An intermediate folder will be created for each feed.")

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> strOption (long "directory" <> short 'd' <> metavar "PATH" <> help "Root directory where files will be created.")
  <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")



main :: IO ()
main = do
  CliOptions directory dryRun <- parseOptions
  message <- getContents <&> fromStrict <&> MsgPack.unpack
  case message of
    Just (Message feed element) -> do
      let content = defaultFileContent feed element
          filePath = defaultFilePath directory feed element
      putStrLn filePath
      unless dryRun $ do
        createDirectoryIfMissing True $ takeDirectory filePath
        writeBinaryFile filePath $ toStreamingByteString content
    _ -> putStrLn "Invalid input" >> exitFailure
  return ()

-- * Default behavior

-- | Generate a path @<root>/<feed title>/<element date>-<element title>.html@, where @<root>@ is the first argument
defaultFilePath :: FilePath -> Feed -> FeedElement -> FilePath
defaultFilePath root feed element = makeValid $ root </> toString title </> fileName <.> "html" where
  date = maybe "" (formatTime defaultTimeLocale "%F-") $ getDate element
  fileName = date <> toString (sanitize $ getTitle element)
  title = sanitize $ getFeedTitle feed
  sanitize = appEndo (mconcat [Endo $ Text.replace (toText [s]) "_" | s <- pathSeparators])
    >>> Text.replace "." "_"
    >>> Text.replace "?" "_"
    >>> Text.replace "!" "_"
    >>> Text.replace "#" "_"

-- | Generate an HTML page, with a title, a header and an article that contains the feed element
defaultFileContent :: Feed -> FeedElement -> Builder
defaultFileContent feed element = renderHtmlBuilder $ docTypeHtml $ do
  H.head $ do
    H.meta ! H.charset "utf-8"
    H.title $ convertText $ getFeedTitle feed <> " | " <> getTitle element
  H.body $ do
    H.h1 $ convertText $ getFeedTitle feed
    H.article $ do
      H.header $ do
        defaultArticleTitle feed element
        defaultArticleAuthor feed element
        defaultArticleDate feed element
      defaultBody feed element


-- * Low-level helpers

defaultArticleTitle :: Feed -> FeedElement -> Html
defaultArticleTitle _ element@(RssElement item) = H.h2 $ maybe id (\uri -> H.a ! H.href uri) link $ convertText $ getTitle element where
  link = withRssURI (convertDoc . prettyURI) <$> itemLink item
defaultArticleTitle _ element@(AtomElement _) = H.h2 $ convertText $ getTitle element

defaultArticleAuthor :: Feed -> FeedElement -> Html
defaultArticleAuthor _ (RssElement item) = unless (Text.null author) $ H.address $ "Published by " >> convertText author where
  author = itemAuthor item
defaultArticleAuthor _ (AtomElement entry) = H.address $ do
  "Published by "
  forM_ (entryAuthors entry) $ \author -> do
    convertDoc $ prettyPerson author
    ", "

defaultArticleDate :: Feed -> FeedElement -> Html
defaultArticleDate _ element = forM_ (getDate element) $ \date -> H.p $ " on " >> H.time (convertDoc $ prettyTime date)


-- | Generate the HTML content for a given feed element
defaultBody :: Feed -> FeedElement -> Html
defaultBody _ element@(RssElement _) = H.p $ preEscapedToHtml $ getContent element
defaultBody _ element@(AtomElement entry) = do
  unless (null links) $ H.p $ do
    "Related links:"
    H.ul $ forM_ links $ \uri -> H.li (H.a ! withAtomURI href uri $ convertAtomURI uri)
  H.p $ preEscapedToHtml $ getContent element
  where links   = map linkHref $ entryLinks entry

href :: URIRef a -> H.Attribute
href = H.href . convertURI

convertAtomURI :: (IsString t) => AtomURI -> t
convertAtomURI = withAtomURI convertURI

convertURI :: (IsString t) => URIRef a -> t
convertURI = convertText . decodeUtf8 . serializeURIRef'

convertText :: (IsString t) => Text -> t
convertText = fromString . toString

convertDoc :: (IsString t) => Doc a -> t
convertDoc = show
