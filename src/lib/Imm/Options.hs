{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Imm.Options where

-- {{{ Imports
import           Imm.Dyre                    as Dyre (Mode (..))
import qualified Imm.Dyre                    as Dyre
import           Imm.Feed
import           Imm.Logger                  as Logger
import           Imm.Prelude
import           Imm.Pretty

import           Options.Applicative.Builder
import           Options.Applicative.Extra
import           Options.Applicative.Types

import           URI.ByteString
-- }}}

-- | Available commands.
data Command = Check (Maybe FeedRef)
             | Import
             | Read (Maybe FeedRef)
             | Rebuild
             | Unread (Maybe FeedRef)
             | Run (Maybe FeedRef)
             | Show (Maybe FeedRef)
             | ShowVersion
             | Subscribe URI (Maybe Text)
             | Unsubscribe (Maybe FeedRef)

deriving instance Eq Command
deriving instance Show Command

instance Pretty Command where
  pretty (Check f)       = "Check feed(s):" <+> pretty f
  pretty Import          = "Import feeds"
  pretty (Read f)        = "Mark feed(s) as read:" <+> pretty f
  pretty Rebuild         = "Rebuild configuration"
  pretty (Unread f)      = "Mark feed(s) as unread:" <+> pretty f
  pretty (Run f)         = "Download new entries from feed(s):" <+> pretty f
  pretty (Show f)        = "Show status for feed(s):" <+> pretty f
  pretty ShowVersion     = "Show program version"
  pretty (Subscribe f _) = "Subscribe to feed:" <+> prettyURI f
  pretty (Unsubscribe f) = "Unsubscribe from feed(s):" <+> pretty f

defaultCommand :: Command
defaultCommand = Show Nothing

-- | Available commandline options.
data CliOptions = CliOptions
  { optionCommand      :: Command
  , optionDyreMode     :: Dyre.Mode
  , optionLogLevel     :: LogLevel
  , optionColorizeLogs :: Bool
  }

-- deriving instance Eq CliOptions
defaultOptions :: CliOptions
defaultOptions = CliOptions defaultCommand Dyre.defaultMode Info True

-- instance Pretty CliOptions where
--     pretty opts = text "ACTION" <> equals <>  $ opts^.command_
--         , ("RECONFIGURATION_MODE=" ++) . show $ opts^.dyreMode_
--         ]
--         ++ catMaybes [("CONFIG=" ++) <$> opts^.configurationLabel_]

parseOptions :: (MonadIO m) => m CliOptions
parseOptions = io $ customExecParser (prefs noBacktrack) (info parser $ progDesc "Convert items from RSS/Atom feeds to mails.")
  where parser = helper <*> optional dyreMasterBinary *> optional dyreDebug *> cliOptions


cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> commands
  <*> (vanillaFlag <|> forceReconfFlag <|> denyReconfFlag <|> pure Dyre.defaultMode)
  <*> (verboseFlag <|> quietFlag <|> logLevel <|> pure Info)
  <*> (colorizeLogs <|> pure True)


commands :: Parser Command
commands = subparser $ mconcat
  [ command "check" . info (Check <$> optional feedRefOption) $ progDesc "Check availability and validity of all feed sources currently configured, without writing any mail."
  , command "import" . info (pure Import) $ progDesc "Import feeds list from an OPML descriptor (read from stdin)."
  , command "read" . info (Read <$> optional feedRefOption) $ progDesc "Mark given feed as read."
  , command "rebuild" . info (pure Rebuild) $ progDesc "Rebuild configuration file."
  , command "run" . info (Run <$> optional feedRefOption) $ progDesc "Update list of feeds."
  , command "show" . info (Show <$> optional feedRefOption) $ progDesc "List all feed sources currently configured, along with their status."
  , command "subscribe" . info subscribeOptions $ progDesc "Subscribe to a feed."
  , command "unread" . info (Unread <$> optional feedRefOption) $ progDesc "Mark given feed as unread."
  , command "unsubscribe" . info unsubscribeOptions $ progDesc "Unsubscribe from a feed."
  , command "version" . info (pure ShowVersion) $ progDesc "Print version."
  ]


-- {{{ Dynamic reconfiguration options
vanillaFlag, forceReconfFlag, denyReconfFlag :: Parser Dyre.Mode
vanillaFlag      = flag' Vanilla $ long "vanilla" <> short '1' <> help "Ignore custom configuration file."
forceReconfFlag  = flag' ForceReconfiguration $ long "force-reconf" <> help "Recompile configuration file before starting the application."
denyReconfFlag   = flag' IgnoreReconfiguration $ long "deny-reconf" <> help "Do not recompile configuration file even if it has changed."

dyreDebug :: Parser Bool
dyreDebug = switch $ long "dyre-debug" <> help "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program."

dyreMasterBinary :: Parser String
dyreMasterBinary = strOption $ long "dyre-master-binary" <> metavar "PATH" <> hidden <> internal <> help "Internal flag used for dynamic reconfiguration."
-- }}}

-- {{{ Log options
verboseFlag, quietFlag, logLevel :: Parser LogLevel
verboseFlag = flag' Logger.Debug $ long "verbose" <> short 'v' <> help "Set log level to DEBUG."
quietFlag   = flag' Logger.Error $ long "quiet" <> short 'q' <> help "Set log level to ERROR."
logLevel    = option auto $ long "log-level" <> short 'l' <> metavar "LOG-LEVEL" <> value Info <> completeWith ["Debug", "Info", "Warning", "Error"] <> help "Set log level. Available values: Debug, Info, Warning, Error."

colorizeLogs :: Parser Bool
colorizeLogs = flag' False $ long "nocolor" <> help "Disable log colorisation."
-- }}}

-- {{{ Other options
configLabelOption :: Parser Text
configLabelOption = option auto $ long "config" <> short 'C' <> metavar "CONFIG" <> help "Use the given configuration for all operations."

subscribeOptions, unsubscribeOptions :: Parser Command
subscribeOptions    = Subscribe <$> uriArgument "URI to subscribe to." <*> optional configLabelOption
unsubscribeOptions  = Unsubscribe <$> optional feedRefOption
-- }}}

-- {{{ Util
uriReader :: ReadM URI
uriReader = eitherReader $ first show . parseURI laxURIParserOptions . encodeUtf8 . fromString

feedRefOption :: Parser FeedRef
feedRefOption = fmap FeedRef $ (Left <$> argument auto (metavar "ID")) <|> (Right <$> argument uriReader (metavar "URI"))

uriArgument :: String -> Parser URI
uriArgument helpText = argument uriReader $ metavar "URI" <> help helpText
-- }}}
