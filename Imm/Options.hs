{-# LANGUAGE TemplateHaskell #-}
-- | Commandline options tools. Designed to be imported as @qualified@.
module Imm.Options (
    CliOptions,
    action,
    configuration,
    feedsList,
    dataDirectory,
    OptionsReader(..),
    Action(..),
    Configuration(..),
    run,
    usage,
) where

-- {{{ Imports
import Imm.Util

import Control.Lens as L  hiding(Action, (??))
import Control.Monad.Reader hiding(mapM_, when)

import Data.Foldable

import Network.URI as N

import Prelude hiding(foldl, log, mapM_)

import System.Console.GetOpt
import System.Environment
-- import System.Environment.XDG.BaseDir
import System.Log as Log
import System.Log.Logger
-- }}}

-- {{{ Types
-- | Mutually exclusive actions.
-- Default is 'PrintHelp'.
data Action = Help | ShowVersion | Recompile | CheckFeeds | ImportFeeds | ListFeeds | MarkAsRead | MarkAsUnread | UpdateFeeds
    deriving(Eq, Show)

instance Default Action where
    def = Help

-- | How dynamic reconfiguration process should behave.
-- Default is 'Normal', that is: use custom configuration file and recompile if change detected.
data Configuration = Normal | Vanilla | ForceReconfiguration | IgnoreReconfiguration
    deriving(Eq, Show)

instance Default Configuration where
    def = Normal


-- | Available commandline options
data CliOptions = CliOptions {
    _action         :: Action,
    _configuration  :: Configuration,
    _dataDirectory :: Maybe FilePath,
    _feedsList      :: [URI],
    _logLevel       :: Log.Priority,
    _dyreDebug      :: Bool}
    deriving(Eq)

makeLenses ''CliOptions

instance Show CliOptions where
    show opts = unwords $ catMaybes [
        return . ("ACTION=" ++) . show $ view action opts,
        return . ("CONFIGURATION=" ++) . show $ view configuration opts,
        null (view feedsList opts) ? Nothing ?? Just ("FEED_URI=[" ++ (unwords . map show $ view feedsList opts) ++ "]"),
        return . ("DATA_DIR=" ++) =<< view dataDirectory opts,
        return . ("LOG_LEVEL=" ++) . show $ view logLevel opts,
        view dyreDebug opts ? Just "DYRE_DEBUG" ?? Nothing]

instance Default CliOptions where
    def = CliOptions {
        _action         = def,
        _configuration  = def,
        _logLevel       = Log.INFO,
        _dataDirectory = Nothing,
        _feedsList      = [],
        _dyreDebug      = False}

-- | 'MonadReader' for 'CliOptions'
class OptionsReader m where
    readOptions :: Simple Lens CliOptions a -> m a

instance (Monad m) => OptionsReader (ReaderT CliOptions m) where
    readOptions l = return . view l =<< ask

instance OptionsReader ((->) CliOptions) where
    readOptions l = view l

-- | Parse commandline options, set the corresponding log level.
run :: (MonadBase IO m) => ReaderT CliOptions m a -> m a
run f = do
    opts <- get
    io . updateGlobalLogger rootLoggerName . setLevel $ view logLevel opts
    io . debugM "imm.options" $ "Commandline options: " ++ show opts
    runReaderT f opts
-- }}}

description :: [OptDescr (CliOptions -> CliOptions)]
description = [
-- Action
    Option "c"     ["check"]              (NoArg (set action CheckFeeds))                   "Check availability and validity of all feed sources currently configured, without writing any mail",
    Option "l"     ["list"]               (NoArg (set action ListFeeds))                    "List all feed sources currently configured, along with their status",
    Option "R"     ["mark-read"]          (NoArg (set action MarkAsRead))                   "Mark every item of processed feeds as read, ie set last update as now without writing any mail",
    Option "U"     ["mark-unread"]        (NoArg (set action MarkAsUnread))                 "Mark every item of processed feeds as unread, ie delete corresponding state files",
    Option "u"     ["update"]             (NoArg (set action UpdateFeeds))                  "Update list of feeds (mostly used option)",
    Option "i"     ["import"]             (NoArg (set action ImportFeeds))                  "Import feeds list from an OPML descriptor (read from stdin)",
    Option "h"     ["help"]               (NoArg (set action Help))                         "Print this help",
    Option "V"     ["version"]            (NoArg (set action ShowVersion))                  "Print version",
    Option "r"     ["recompile"]          (NoArg (set action Recompile))                    "Only recompile configuration",
-- Dynamic configuration
    Option "1"     ["vanilla"]            (NoArg (set configuration Vanilla))               "Do not read custom configuration file",
    Option []        ["force-reconf"]       (NoArg (set configuration ForceReconfiguration))  "Recompile configuration before starting the program",
    Option []        ["deny-reconf"]        (NoArg (set configuration IgnoreReconfiguration)) "Do not recompile configuration even if it has changed",
    Option []        ["dyre-debug"]         (NoArg id)                                        "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program",
-- Log level
    Option "q"     ["quiet"]              (NoArg (set logLevel Log.ERROR))                  "Do not print any log",
    Option "v"     ["verbose"]            (NoArg (set logLevel Log.DEBUG))                  "Print detailed logs",
-- Misc
    Option "d"     ["database"]           (ReqArg (set dataDirectory . Just) "PATH")        "Where feeds' state (last update time) will be stored"]

-- | Usage text (printed when using 'Help' action)
usage :: String
usage = usageInfo "Usage: imm [OPTIONS] [URI]\n\nConvert items from RSS/Atom feeds to maildir entries. If one or more URI(s) are given, they will be processed instead of the feeds list from configuration\n" description

-- | Get and parse commandline options
get :: (MonadBase IO m) => m CliOptions
get = io $ do
    options <- getOpt' Permute description <$> getArgs
    case options of
        (opts, input, _, []) -> do
            let (errors, valids) = partitionEithers $ map (\uri -> maybe (Left $ "Invalid URI given in commandline: " ++ uri) Right $ N.parseURI uri) input
            unless (null errors) $ io . putStrLn $ unlines errors
            return $ set feedsList valids  (foldl (flip id) def opts)
        (_, _, _, _)         -> return def
