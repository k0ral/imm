{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
module Input where

-- {{{ Imports
import           Imm.Feed
import           Imm.Logger          as Logger
import           Imm.Pretty
import qualified Paths_imm           as Package

import           Data.List
import qualified Data.Set            as Set
import           Data.Version
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.Info
import           URI.ByteString
-- }}}

-- * Types

data ProgramInput = ProgramInput
  { inputCommand          :: Command
  , inputLogLevel         :: LogLevel
  , inputReadOnlyDatabase :: Bool
  , inputCallbacksFile    :: FilePath
  } deriving(Eq, Ord, Show)

instance Pretty ProgramInput where
  pretty input = encloseSep "{ " " }" ", "
    [ "command" <+> equals <+> pretty (inputCommand input)
    , "log level" <+> equals <+> pretty (inputLogLevel input)
    , "database read-only" <+> equals <+> pretty (inputReadOnlyDatabase input)
    , "callbacks file" <+> equals <+> pretty (inputCallbacksFile input)
    ]


data Command = Subscribe FeedLocation (Set Text)
             | Describe FeedQuery
             | Reset FeedQuery
             | Run FeedQuery CallbackMode
             | Unsubscribe FeedQuery

deriving instance Eq Command
deriving instance Ord Command
deriving instance Show Command

instance Pretty Command where
  pretty (Subscribe f _) = "Subscribe to feed:" <+> pretty f
  pretty (Describe f)    = "Describe feed" <+> pretty f
  pretty (Reset q)       = "Mark feeds as unprocessed:" <+> pretty q
  pretty (Run q c)       = "Download new entries from:" <+> (if c == DisableCallbacks then space <> brackets "callbacks disabled" else mempty) <+> pretty q
  pretty (Unsubscribe q) = "Unsubscribe from feeds:" <+> pretty q


data CallbackMode = DisableCallbacks | EnableCallbacks
  deriving(Eq, Ord, Read, Show)


-- * Option parsers

parseOptions :: (MonadIO m) => m ProgramInput
parseOptions = io $ do
  defaultCallbacksFile <- getXdgDirectory XdgConfig $ "imm" </> "callbacks.dhall"
  execParser $ info (allOptions defaultCallbacksFile <**> helper <**> versionPrinter) $ progDesc description

description :: String
description = "Execute arbitrary callbacks for each element of RSS/Atom feeds."

allOptions :: FilePath -> Parser ProgramInput
allOptions defaultCallbacksFile = ProgramInput
  <$> commandParser
  <*> (verboseFlag <|> quietFlag <|> logLevel <|> pure Info)
  <*> readOnlyDatabase
  <*> (callbacksFileOption <|> pure defaultCallbacksFile)

commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ command "subscribe" $ info subscribeCommand $ progDesc "Subscribe to a feed."
  , command "add" $ info subscribeCommand $ progDesc "Alias for subscribe."

  , command "run" $ info runCommand $ progDesc "Update list of feeds."
  , command "describe" $ info describeCommand $ progDesc "Show details about given feed."
  , command "show" $ info describeCommand $ progDesc "Alias for describe."
  , command "list" $ info (pure $ Describe QueryAll) $ progDesc "Alias for describe --all ."
  , command "reset" $ info resetCommand $ progDesc "Mark given feed as unprocessed."
  , command "unsubscribe" $ info unsubscribeCommand $ progDesc "Unsubscribe from a feed."
  , command "remove" $ info unsubscribeCommand $ progDesc "Alias for unsubscribe."
  ]

-- {{{ Commands
describeCommand :: Parser Command
describeCommand = Describe <$> feedQueryParser

subscribeCommand :: Parser Command
subscribeCommand    = Subscribe <$> feedLocationParser <*> (Set.fromList <$> many tagOption)

unsubscribeCommand :: Parser Command
unsubscribeCommand  = Unsubscribe <$> feedQueryParser

runCommand :: Parser Command
runCommand = Run
  <$> feedQueryParser
  <*> flag EnableCallbacks DisableCallbacks (long "no-callbacks" <> help "Disable callbacks.")

resetCommand :: Parser Command
resetCommand = Reset <$> feedQueryParser
-- }}}

-- {{{ Log options
verboseFlag, quietFlag, logLevel :: Parser LogLevel
verboseFlag = flag' Logger.Debug $ long "verbose" <> short 'v' <> help "Set log level to DEBUG."
quietFlag   = flag' Logger.Error $ long "quiet" <> short 'q' <> help "Set log level to ERROR."
logLevel    = option auto $ long "log-level" <> short 'l' <> metavar "LOG-LEVEL" <> value Info <> completeWith ["Debug", "Info", "Warning", "Error"] <> help "Set log level. Available values: Debug, Info, Warning, Error."
-- }}}

-- {{{ Other options
readOnlyDatabase :: Parser Bool
readOnlyDatabase = switch $ long "read-only" <> help "Disable database writes."

tagOption :: Parser Text
tagOption = option auto $ long "tag" <> short 't' <> metavar "TAG" <> help "Set the given tag."

callbacksFileOption :: Parser FilePath
callbacksFileOption = strOption $ long "callbacks" <> short 'c' <> metavar "FILE" <> help "Dhall configuration file for callbacks"
-- }}}

-- {{{ Util
uriReader :: ReadM URI
uriReader = eitherReader $ first show . parseURI laxURIParserOptions . encodeUtf8 @Text . fromString

feedLocationParser :: Parser FeedLocation
feedLocationParser = FeedLocation <$> argument uriReader (metavar "URI" <> help "URI to RSS/Atom document, or to HTML page.") <*> strOption (long "title" <> short 'T' <> value mempty <> help "Title used to disambiguate multiple alternate links in HTML page.")

feedQueryParser :: Parser FeedQuery
feedQueryParser = (QueryByUID <$> argument auto (metavar "TARGET")) <|> allFeeds where
  allFeeds = flag' QueryAll $ short 'a' <> long "all" <> help "Run action on all subscribed feeds."

uriArgument :: String -> Parser URI
uriArgument helpText = argument uriReader $ metavar "URI" <> help helpText

versionPrinter :: Parser (a -> a)
versionPrinter = infoOption
  (Data.List.unlines ["imm-" <> showVersion Package.version, "built with " <> compilerName <> "-" <> showVersion compilerVersion])
  (long "version" <> short 'V' <> help "Print versions.")
-- }}}
