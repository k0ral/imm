{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}
-- | Commandline options tools. Designed to be imported as @qualified@.
module Imm.Options where

-- {{{ Imports
import Imm.Util

import Control.Conditional
import Control.Lens as L  hiding((??))
import Control.Monad.Base
import Control.Monad.Reader hiding(when)

import Data.Default
import Data.Either
import Data.Functor
import Data.List
import Data.Maybe

import Network.URI as N

import Prelude hiding(log)

import System.Console.GetOpt
import System.Environment
import System.Environment.XDG.BaseDir
import System.IO
-- }}}

-- {{{ Types
-- | Available commandline options (cf @imm -h@)
data CliOptions = CliOptions {
    _stateDirectory :: Maybe FilePath,
    _check          :: Bool,
    _feedList       :: [URI],
    _importOPML     :: Maybe FilePath,
    _list           :: Bool,
    _markAsRead     :: Bool,
    _markAsUnread   :: Bool,
    _update         :: Bool,
    _help           :: Bool,
    _quiet          :: Bool,
    _verbose        :: Bool,
    _version        :: Bool,
    _vanilla        :: Bool,
    _recompile      :: Bool,
    _denyReconf     :: Bool,
    _forceReconf    :: Bool,
    _dyreDebug      :: Bool}
    deriving(Eq)

makeLenses ''CliOptions

instance Show CliOptions where
    show opts = intercalate " " $ catMaybes [
        null (view feedList opts) ? Nothing ?? Just ("FEED_URI=[" ++ (intercalate " " . map show $ view feedList opts) ++ "]"),
        return . ("IMPORT_OPML=" ++) =<< view importOPML opts,
        return . ("STATE_DIR=" ++) =<< view stateDirectory opts,
        view check        opts ? Just "CHECK"                 ?? Nothing,
        view list         opts ? Just "LIST"                  ?? Nothing,
        view markAsRead   opts ? Just "MARK_READ"             ?? Nothing,
        view markAsUnread opts ? Just "MARK_UNREAD"           ?? Nothing,
        view update       opts ? Just "UPDATE"                ?? Nothing,
        view help         opts ? Just "HELP"                  ?? Nothing,
        view quiet        opts ? Just "QUIET"                 ?? Nothing,
        view verbose      opts ? Just "VERBOSE"               ?? Nothing,
        view version      opts ? Just "VERSION"               ?? Nothing,
        view vanilla      opts ? Just "VANILLA"               ?? Nothing,
        view recompile    opts ? Just "RECOMPILE"             ?? Nothing,
        view denyReconf   opts ? Just "DENY_RECONFIGURATION"  ?? Nothing,
        view forceReconf  opts ? Just "FORCE_RECONFIGURATION" ?? Nothing,
        view dyreDebug    opts ? Just "DYRE_DEBUG"            ?? Nothing]

instance Default CliOptions where
    def = CliOptions {
        _stateDirectory = Nothing,
        _check          = False,
        _feedList       = [],
        _importOPML     = Nothing,
        _list           = False,
        _markAsRead     = False,
        _markAsUnread   = False,
        _update         = False,
        _help           = False,
        _quiet          = False,
        _verbose        = False,
        _version        = False,
        _vanilla        = False,
        _recompile      = False,
        _denyReconf     = False,
        _forceReconf    = False,
        _dyreDebug      = False}

-- | 'MonadReader' for 'CliOptions'
class OptionsReader m where
    readOptions :: Simple Lens CliOptions a -> m a

instance (Monad m) => OptionsReader (ReaderT CliOptions m) where
    readOptions l = return . view l =<< ask

instance OptionsReader ((->) CliOptions) where
    readOptions l = view l
-- }}}

description :: [OptDescr (CliOptions -> CliOptions)]
description = [
    Option ['s']     ["state"]              (ReqArg (\v -> set stateDirectory (Just v)) "PATH") "Where feeds' state (last update time) will be stored",
    Option ['c']     ["check"]              (NoArg (set check True))                        "Check availability and validity of all feed sources currently configured, without writing any mail",
    Option ['l']     ["list"]               (NoArg (set list True))                         "List all feed sources currently configured, along with their status",
    Option ['R']     ["mark-read"]          (NoArg (set markAsRead True))                   "Mark every item of processed feeds as read, ie set last update as now without writing any mail",
    Option ['U']     ["mark-unread"]        (NoArg (set markAsUnread True))                 "Mark every item of processed feeds as unread, ie delete corresponding state files",
    Option ['u']     ["update"]             (NoArg (set update True))                       "Update list of feeds (mostly used option)",
    Option ['i']     ["import"]             (ReqArg (\v -> set importOPML (Just v)) "PATH") "Import feeds list from an OPML descriptor (read from stdin)",
    Option ['h']     ["help"]               (NoArg (set help True))                         "Print this help",
    Option ['q']     ["quiet"]              (NoArg (set quiet True))                        "Do not print any log",
    Option ['v']     ["verbose"]            (NoArg (set verbose True))                      "Print detailed logs",
    Option ['V']     ["version"]            (NoArg (set version True))                      "Print version",
    Option ['1']     ["vanilla"]            (NoArg (set vanilla True))                      "Do not read custom configuration file",
    Option ['r']     ["recompile"]          (NoArg (set recompile True))                    "Only recompile configuration",
    Option []        ["force-reconf"]       (NoArg id)                                      "Recompile configuration before starting the program",
    Option []        ["deny-reconf"]        (NoArg id)                                      "Do not recompile configuration even if it has changed",
    Option []        ["dyre-debug"]         (NoArg id)                                      "Use './cache/' as the cache directory and ./ as the configuration directory. Useful to debug the program"]

-- | Usage text (cf @hbro -h@)
usage :: String
usage = usageInfo "Usage: imm [OPTIONS] [URI]\n\nConvert items from RSS/Atom feeds to maildir entries. If one or more URI(s) are given, they will be processed instead of the feeds list from configuration\n" description

-- | Get and parse commandline options
get :: (MonadBase IO m) => m CliOptions
get = io $ do
    options <- getOpt' Permute description <$> getArgs
    case options of
        (opts, input, _, []) -> do
            let (errors, valids) = partitionEithers $ map (\uri -> maybe (Left $ "Invalid URI given in commandline: " ++ uri) Right $ N.parseURI uri) input
            when (not $ null errors) $ io . putStrLn $ unlines errors
            return $ set feedList valids (foldl (flip id) def opts)
        (_, _, _, _)         -> return def

-- | Print logs with arbitrary importance
log, logE, logV :: (MonadBase IO m, OptionsReader m) => String -> m ()
log  = whenM (not <$> readOptions quiet) . io . putStrLn
logE = whenM (not <$> readOptions quiet) . io . hPutStr stderr
logV = whenM (readOptions verbose) . io . putStrLn


getStateDirectory :: (OptionsReader m, MonadBase IO m) => m FilePath
getStateDirectory = do
    stateFromOptions <- readOptions stateDirectory
    case stateFromOptions of
        Just x -> return x
        _      -> getUserConfigDir "imm" >/> "state"
