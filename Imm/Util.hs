{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Imm.Util where

-- {{{ Imports
import Imm.Types

import qualified Control.Exception as E
import Control.Monad.Error
--import Control.Monad.IO.Class

import Data.Maybe
import qualified Data.Time as T
import Data.Time.RFC2822
import Data.Time.RFC3339

import qualified Network.URI as N

import System.Console.CmdArgs
import System.Directory
import System.Environment.XDG.BaseDir
import System.Locale

import qualified Text.Feed.Import as F
import Text.Feed.Types
-- }}}

-- {{{ Monadic utilities
-- | Shortcut to liftIO
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Monad-agnostic version of Control.Exception.try
try :: (MonadIO m, MonadError ImmError m) => IO a -> m a
try = (io . E.try) >=> either (throwError . IOE) return 
-- }}}

-- | Print logs with arbitrary importance.
logNormal, logVerbose :: MonadIO m => String -> m ()
logNormal  = io . whenNormal . putStrLn
logVerbose = io . whenLoud . putStrLn

-- | Evaluate given function while replacing directory variables appropriately for the current system
resolve :: MonadIO m => (RefDirs -> a) -> m a
resolve f = io $ do
    homeDir   <- getHomeDirectory
    tmpDir    <- getTemporaryDirectory
    configDir <- getUserConfigDir "imm"
    dataDir   <- getUserDataDir   "imm"
    
    return . f $ RefDirs homeDir tmpDir configDir dataDir

-- {{{ Parsing functions should use be monad agnostic    
parseDate :: String -> Maybe T.UTCTime
parseDate date = listToMaybe . map T.zonedTimeToUTC . catMaybes . map ((flip ($)) date) $ [readRFC2822, readRFC3339]

parseFeedString :: MonadError ImmError m => String -> m Feed
parseFeedString x = maybe (throwError . ParseFeedError $ show x) return $ F.parseFeedString x

parseURI :: (MonadError ImmError m) => String -> m N.URI
parseURI uri = maybe (throwError $ ParseUriError uri) return $ N.parseURI uri

parseTime :: (MonadError ImmError m) => String -> m T.UTCTime
parseTime string = maybe (throwError $ ParseTimeError string) return $ T.parseTime defaultTimeLocale "%c" string
-- }}}
