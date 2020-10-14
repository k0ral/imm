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
import qualified Data.Text               as Text (replace)
import           Data.Time
import           Options.Applicative
import           System.Directory        (createDirectoryIfMissing)
import           System.Exit             (ExitCode)
import           System.FilePath
import           System.Process.Typed
import           URI.ByteString.Extended
-- }}}

data CliOptions = CliOptions
  { _directory :: FilePath
  , _dryRun    :: Bool
  } deriving (Eq, Ord, Read, Show)

parseOptions :: MonadIO m => m CliOptions
parseOptions = io $ customExecParser defaultPrefs (info cliOptions $ progDesc "Download a self-contained HTML file, using monolith, for each new RSS/Atom item. An intermediate folder will be created for each feed.")

cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> strOption (long "directory" <> short 'd' <> metavar "PATH" <> help "Root directory where files will be created.")
  <*> switch (long "dry-run" <> help "Disable all I/Os, except for logs.")


main :: IO ()
main = do
  CliOptions directory dryRun <- parseOptions
  input <- getContents <&> eitherDecode

  case input :: Either String CallbackMessage of
    Right (CallbackMessage feedDefinition item) -> do
      let filePath = defaultFilePath directory feedDefinition item
      putStrLn filePath
      unless dryRun $ do
        createDirectoryIfMissing True $ takeDirectory filePath
        case getMainLink item of
          Just link -> downloadPage filePath (_linkURI link) >>= exitWith
          _         -> putStrLn ("No main link in item " <> show (prettyName item)) >> exitFailure
    Left e -> putStrLn ("Invalid input: " <> e) >> exitFailure
  return ()

-- * Default behavior

downloadPage :: FilePath -> AnyURI -> IO ExitCode
downloadPage filePath uri = runProcess $ proc "monolith" ["-o", filePath, show $ pretty uri]
  & setStdin nullStream
  & setStdout inherit
  & setStderr nullStream

-- | Generate a path @<root>/<feed title>/<element date>-<element title>.html@, where @<root>@ is the first argument
defaultFilePath :: FilePath -> FeedDefinition -> FeedItem -> FilePath
defaultFilePath root feedDefinition element = makeValid $ root </> toString title </> fileName <.> "html" where
  date = maybe "" (formatTime defaultTimeLocale "%F-") $ _itemDate element
  fileName = date <> toString (sanitize $ _itemTitle element)
  title = sanitize $ _feedTitle feedDefinition
  sanitize = appEndo (mconcat [Endo $ Text.replace (toText [s]) "_" | s <- pathSeparators])
    >>> Text.replace "." "_"
    >>> Text.replace "?" "_"
    >>> Text.replace "!" "_"
    >>> Text.replace "#" "_"
