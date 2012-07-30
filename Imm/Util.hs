{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Imm.Util where

-- {{{ Imports
import Imm.Types

import qualified Control.Exception as E
import Control.Monad.Error
--import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text.Lazy.Encoding hiding(decodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Time as T
import Data.Time.RFC2822
import Data.Time.RFC3339

import Network.Browser as N
import Network.HTTP
import Network.URI as N

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

-- {{{ Monad-agnostic version of various error-prone functions    
decodeUtf8 :: MonadError ImmError m => B.ByteString -> m T.Text
decodeUtf8 = either (throwError . UnicodeError) return . decodeUtf8'


parseDate :: String -> Maybe T.UTCTime
parseDate date = listToMaybe . map T.zonedTimeToUTC . catMaybes . flip map [readRFC2822, readRFC3339, T.parseTime defaultTimeLocale "%a, %d %b %G %T", T.parseTime defaultTimeLocale "%Y-%m-%d", T.parseTime defaultTimeLocale "%e %b %Y"] $ \f -> f . T.unpack . T.strip . T.pack $ date

parseFeedString :: MonadError ImmError m => String -> m Feed
parseFeedString x = maybe (throwError . ParseFeedError $ show x) return $ F.parseFeedString x

parseURI :: (MonadError ImmError m) => String -> m URI
parseURI uri = maybe (throwError $ ParseUriError uri) return $ N.parseURI uri

parseTime :: (MonadError ImmError m) => String -> m T.UTCTime
parseTime string = maybe (throwError $ ParseTimeError string) return $ T.parseTime defaultTimeLocale "%c" string

request :: Request B.ByteString -> BrowserAction (HandleStream B.ByteString) (URI, Response B.ByteString)
request r = io $ (E.catch :: IO a -> (IOError -> IO a) -> IO a)
    (browse $ N.request r)
    (\e -> return (rqURI r, Response (4,0,4) ("Unable to connect to " ++ show (rqURI r)) [] B.empty))    
-- }}}
