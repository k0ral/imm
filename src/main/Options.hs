{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
module Options where

-- {{{ Imports
import           Imm.Feed
import           Imm.Logger                     as Logger
import           Imm.Pretty
import qualified Paths_imm                      as Package

import           Data.List
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Version
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.Info
import           URI.ByteString
-- }}}

-- * Types

data AllOptions = AllOptions
  { optionCommand :: Command
  , optionGlobal  :: GlobalOptions
  } deriving(Eq, Show)


data GlobalOptions = GlobalOptions
  { optionLogLevel         :: LogLevel
  , optionColorizeLogs     :: Bool
  , optionReadOnlyDatabase :: Bool
  , optionCallbacksFile    :: FilePath
  } deriving(Eq, Ord, Read, Show)


data Command = Import | Subscribe URI (Set Text) | List | ShowFeed FeedRef | OnFeedRef (Maybe FeedRef) CommandOnFeedRef

deriving instance Eq Command
deriving instance Ord Command
deriving instance Show Command

instance Pretty Command where
  pretty Import          = "Import feeds"
  pretty (Subscribe f _) = "Subscribe to feed:" <+> prettyURI f
  pretty (OnFeedRef t c) = pretty c <> ":" <+> pretty t
  pretty List            = "List all feeds"
  pretty (ShowFeed f)    = "Show details of feed" <+> pretty f


data CallbackMode = DisableCallbacks | EnableCallbacks
  deriving(Eq, Ord, Read, Show)


data CommandOnFeedRef = Reset | Run CallbackMode | Unsubscribe
  deriving(Eq, Ord, Read, Show)

instance Pretty CommandOnFeedRef where
  pretty Reset       = "Mark feed as unprocessed"
  pretty (Run c)     = "Download new entries from feeds" <> (if c == DisableCallbacks then space <> brackets "callbacks disabled" else mempty)
  pretty Unsubscribe = "Unsubscribe from feed"

-- * Option parsers

parseOptions :: (MonadIO m) => m AllOptions
parseOptions = io $ do
  defaultCallbacksFile <- getXdgDirectory XdgConfig $ "imm" </> "callbacks.dhall"
  customExecParser defaultPrefs $ info (allOptions defaultCallbacksFile <**> helper <**> versionPrinter) $ progDesc description

description :: String
description = "Execute arbitrary callbacks for each element of RSS/Atom feeds."

allOptions :: FilePath -> Parser AllOptions
allOptions defaultCallbacksFile = AllOptions
  <$> commandParser
  <*> globalOptions defaultCallbacksFile

globalOptions :: FilePath -> Parser GlobalOptions
globalOptions defaultCallbacksFile = GlobalOptions
  <$> (verboseFlag <|> quietFlag <|> logLevel <|> pure Info)
  <*> colorizeLogs
  <*> readOnlyDatabase
  <*> (callbacksFileOption <|> pure defaultCallbacksFile)

commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ command "add" $ info subscribeOptions $ progDesc "Alias for subscribe."
  , command "import" $ info (pure Import) $ progDesc "Import feeds list from an OPML descriptor (read from stdin)."
  , command "subscribe" $ info subscribeOptions $ progDesc "Subscribe to a feed."

  , command "remove" $ info unsubscribeOptions $ progDesc "Alias for unsubscribe."
  , command "run" $ info (OnFeedRef <$> optional feedRefOption <*> runOptions) $ progDesc "Update list of feeds."
  , command "show" $ info (ShowFeed <$> feedRefOption) $ progDesc "Show details about given feed."
  , command "list" $ info (pure List) $ progDesc "List all feed sources currently configured, along with their status."
  , command "reset" $ info (OnFeedRef <$> optional feedRefOption <*> pure Reset) $ progDesc "Mark given feed as unprocessed."
  , command "unsubscribe" $ info unsubscribeOptions $ progDesc "Unsubscribe from a feed."
  ]

-- {{{ Log options
verboseFlag, quietFlag, logLevel :: Parser LogLevel
verboseFlag = flag' Logger.Debug $ long "verbose" <> short 'v' <> help "Set log level to DEBUG."
quietFlag   = flag' Logger.Error $ long "quiet" <> short 'q' <> help "Set log level to ERROR."
logLevel    = option auto $ long "log-level" <> short 'l' <> metavar "LOG-LEVEL" <> value Info <> completeWith ["Debug", "Info", "Warning", "Error"] <> help "Set log level. Available values: Debug, Info, Warning, Error."

colorizeLogs :: Parser Bool
colorizeLogs = flag True False $ long "nocolor" <> help "Disable log colorisation."
-- }}}

-- {{{ Other options
readOnlyDatabase :: Parser Bool
readOnlyDatabase = switch $ long "read-only" <> help "Disable database writes."

runOptions :: Parser CommandOnFeedRef
runOptions = Run <$> flag EnableCallbacks DisableCallbacks (long "no-callbacks" <> help "Disable callbacks.")

tagOption :: Parser Text
tagOption = option auto $ long "tag" <> short 't' <> metavar "TAG" <> help "Set the given tag."

subscribeOptions, unsubscribeOptions :: Parser Command
subscribeOptions    = Subscribe <$> uriArgument "URI to subscribe to." <*> (Set.fromList <$> many tagOption)
unsubscribeOptions  = OnFeedRef <$> optional feedRefOption <*> pure Unsubscribe

callbacksFileOption :: Parser FilePath
callbacksFileOption = strOption $ long "callbacks" <> short 'c' <> metavar "FILE" <> help "Dhall configuration file for callbacks"
-- }}}

-- {{{ Util
uriReader :: ReadM URI
uriReader = eitherReader $ first show . parseURI laxURIParserOptions . encodeUtf8 @Text . fromString

feedRefOption :: Parser FeedRef
feedRefOption = argument ((ByUID <$> auto) <|> (ByURI <$> uriReader)) $ metavar "TARGET"

uriArgument :: String -> Parser URI
uriArgument helpText = argument uriReader $ metavar "URI" <> help helpText

versionPrinter :: Parser (a -> a)
versionPrinter = infoOption
  (Data.List.unlines ["imm-" <> showVersion Package.version, "built with " <> compilerName <> "-" <> showVersion compilerVersion])
  (long "version" <> short 'V' <> help "Print versions.")
-- }}}
