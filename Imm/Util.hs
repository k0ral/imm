{-# LANGUAGE NoMonomorphismRestriction #-}
module Imm.Util where

-- {{{ Imports
import Imm.Types

import Codec.Binary.UTF8.String

--import Control.Exception
import Control.Monad.Error
--import Control.Monad.IO.Class

import Data.Maybe
import Data.Time
import Data.Time.RFC2822
import Data.Time.RFC3339

--import Network.URI

import System.Console.CmdArgs
import System.Directory
import System.Environment.XDG.BaseDir
-- }}}

io :: MonadIO m => IO a -> m a
io = liftIO

logNormal, logVerbose :: String -> IO ()
logNormal  = whenNormal . putStrLn
logVerbose = whenLoud . putStrLn

resolve :: (RefDirs -> a) -> IO a
resolve f = do
    homeDir   <- getHomeDirectory
    tmpDir    <- getTemporaryDirectory
    configDir <- getUserConfigDir "imm"
    dataDir   <- getUserDataDir   "imm"
    
    return . f $ RefDirs homeDir tmpDir configDir dataDir

escapeFileName :: Char -> String
escapeFileName '/' = "|"
escapeFileName x   = x:[]
    
parseDate :: String -> Maybe UTCTime
parseDate date = listToMaybe . map zonedTimeToUTC . catMaybes . map ((flip ($)) date) $ [readRFC2822, readRFC3339]

decodeIfNeeded :: String -> String
decodeIfNeeded text = case isUTF8Encoded text of
    False -> text
    _     -> decodeString text
