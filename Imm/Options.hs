{-# LANGUAGE TemplateHaskell #-}
module Imm.Options (
    CliOptions,
    action,
    dyreMode,
    feedsList,
    dataDirectory,
    logLevel,
    Action(..),
    get,
    usage,
) where

-- {{{ Imports
import Imm.Dyre as Dyre
import qualified Imm.Feed as Feed
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
-- }}}

-- {{{ Types
-- | Mutually exclusive actions.
-- Default is 'PrintHelp'.
data Action = Help | ShowVersion | Recompile | Import | Run Feed.Action
    deriving(Eq, Show)

instance Default Action where
    def = Help

-- | Available commandline options
data CliOptions = CliOptions {
    _action         :: Action,
    _dyreMode       :: Dyre.Mode,
    _dataDirectory  :: Maybe FilePath,
    _feedsList      :: [URI],
    _logLevel       :: Log.Priority}
    deriving(Eq)

makeLenses ''CliOptions

instance Show CliOptions where
    show opts = unwords $ catMaybes [
        return . ("ACTION=" ++) . show $ view action opts,
        return . ("RECONFIGURATION_MODE=" ++) . show $ view dyreMode opts,
        null (view feedsList opts) ? Nothing ?? Just ("FEED_URI=[" ++ (unwords . map show $ view feedsList opts) ++ "]"),
        return . ("DATA_DIR=" ++) =<< view dataDirectory opts,
        return $ "LOG_LEVEL=" ++ show (view logLevel opts)]

instance Default CliOptions where
    def = CliOptions {
        _action        = def,
        _dyreMode      = def,
        _logLevel      = Log.INFO,
        _dataDirectory = Nothing,
        _feedsList     = []}
-- }}}

description :: [OptDescr (CliOptions -> CliOptions)]
description = [
-- Action
    Option "c"     ["check"]              (NoArg (set action $ Run Feed.Check))             "Check availability and validity of all feed sources currently configured, without writing any mail",
    Option "l"     ["list"]               (NoArg (set action $ Run Feed.ShowStatus))        "List all feed sources currently configured, along with their status",
    Option "R"     ["mark-read"]          (NoArg (set action $ Run Feed.MarkAsRead))        "Mark every item of processed feeds as read, ie set last update as now without writing any mail",
    Option "U"     ["mark-unread"]        (NoArg (set action $ Run Feed.MarkAsUnread))      "Mark every item of processed feeds as unread, ie delete corresponding state files",
    Option "u"     ["update"]             (NoArg (set action $ Run Feed.Update))            "Update list of feeds (mostly used option)",
    Option "i"     ["import"]             (NoArg (set action Import))                       "Import feeds list from an OPML descriptor (read from stdin)",
    Option "h"     ["help"]               (NoArg (set action Help))                         "Print this help",
    Option "V"     ["version"]            (NoArg (set action ShowVersion))                  "Print version",
    Option "r"     ["recompile"]          (NoArg (set action Recompile))                    "Only recompile configuration",
-- Dynamic configuration
    Option "1"     ["vanilla"]            (NoArg (set dyreMode Vanilla))                    "Do not read custom configuration file",
    Option []      ["force-reconf"]       (NoArg (set dyreMode ForceReconfiguration))       "Recompile configuration before starting the program",
    Option []      ["deny-reconf"]        (NoArg (set dyreMode IgnoreReconfiguration))      "Do not recompile configuration even if it has changed",
    Option []      ["dyre-debug"]         (NoArg id)                                        "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program",
-- Log level
    Option "q"     ["quiet"]              (NoArg (set logLevel Log.ERROR))                  "Do not print any log",
    Option "v"     ["verbose"]            (NoArg (set logLevel Log.DEBUG))                  "Print detailed logs",
-- Misc
    Option "d"     ["database"]           (ReqArg (set dataDirectory . Just) "PATH")        "Where feeds' state (last update time) will be stored"]

-- | Usage text (printed when using 'Help' action)
usage :: String
usage = flip usageInfo description . unlines $
    "Usage: imm [OPTIONS] [URI]":
    "":
    "Convert items from RSS/Atom feeds to maildir entries.":
    "If one or more URI(s) are given, they will be processed instead of the feeds list from configuration.":[]

-- | Get and parse commandline options
get :: (MonadBase IO m) => m CliOptions
get = io $ do
    parsedArgs <- getOpt' Permute description <$> getArgs
    case parsedArgs of
        (opts, input, _, []) -> do
            let (errors, valids) = partitionEithers $ map parseURI' input
            unless (null errors) $ io . putStrLn $ unlines errors
            return $ set feedsList valids  (foldl (flip id) def opts)
        (_, _, _, _)         -> return def
  where
    parseURI' uri = maybe (Left $ "Invalid URI given in commandline: " ++ uri) Right $ N.parseURI uri
