{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Download a self-contained HTML file, using monolith, related to the input RSS/Atom item.
--
-- Meant to be use as a callback for imm.
-- {{{ Imports
import           Imm.Callback
import           Imm.Feed
import           Imm.Link
import           Imm.Pretty

import           Data.Aeson
import           Data.ByteString.Lazy    (getContents)
import qualified Data.Text               as Text (null, replace, unpack)
import           Data.Time
import           Options.Applicative
import           System.Directory        (createDirectoryIfMissing)
import           System.Exit             (ExitCode)
import           System.FilePath
import           System.Process.Typed
import           URI.ByteString.Extended
-- }}}

data CliOptions = CliOptions
  { _directory        :: FilePath
  , _dryRun           :: Bool
  , _forwardArguments :: [String]
  } deriving (Eq, Ord, Read, Show)

parseOptions :: MonadIO m => m CliOptions
parseOptions = io $ execParser $ info (cliOptions <**> helper) $ progDesc description <> forwardOptions where
  description = "Download a self-contained HTML file, using monolith, for each new RSS/Atom item. An intermediate folder will be created for each feed."

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> strOption (long "directory" <> short 'd' <> metavar "PATH" <> help "Root directory where files will be created.")
  <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")
  <*> many (strArgument $ metavar "ARG" <> help "Argument to forward to monolith.")


main :: IO ()
main = do
  CliOptions rootDirectory dryRun forwardArguments <- parseOptions
  input <- getContents <&> eitherDecode

  case input :: Either String CallbackMessage of
    Right (CallbackMessage feedLocation feedDefinition item) -> do
      let filePath = defaultFilePath rootDirectory feedLocation feedDefinition item
      putStrLn filePath
      unless dryRun $ do
        createDirectoryIfMissing True $ takeDirectory filePath
        case getMainLink item of
          Just link -> downloadPage forwardArguments filePath (_linkURI link) >>= exitWith
          _         -> putStrLn ("No main link in item " <> show (prettyName item)) >> exitFailure
    Left e -> putStrLn ("Invalid input: " <> e) >> exitFailure
  return ()

-- * Default behavior

downloadPage :: [String] -> FilePath -> AnyURI -> IO ExitCode
downloadPage forwardArguments filePath uri = runProcess $ proc "monolith" arguments
  & setStdin nullStream
  & setStdout inherit
  & setStderr inherit
  where arguments = ["-o", filePath, show $ pretty uri] <> forwardArguments

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
