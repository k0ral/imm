module Imm.Database where

-- {{{ Imports
import Imm.Error
import Imm.Options
import Imm.Util

import Control.Monad.Base
import Control.Monad.Error

import Data.Time hiding(parseTime)
import Data.Time.Clock.POSIX

import Network.URI

import System.Directory
import System.FilePath
import System.Locale
import System.IO
-- }}}

-- | A state file stores the last check time for a single feed, identified with its 'URI'.
getStateFile :: URI -> FilePath
getStateFile feedUri@URI{ uriAuthority = Just auth } = toFileName =<< ((++ uriQuery feedUri) . (++ uriPath feedUri) . uriRegName $ auth)
getStateFile feedUri = show feedUri >>= toFileName

-- | Remove forbidden characters in a filename.
toFileName :: Char -> String
toFileName '/' = "."
toFileName '?' = "."
toFileName x = [x]

-- | Read the last check time in the state file.
getLastCheck :: (OptionsReader m, MonadBase IO m) => URI -> m UTCTime
getLastCheck feedUri = do
    directory <- getStateDirectory
    result    <- runErrorT $ do
        content <- try $ readFile (directory </> fileName)
        parseTime content

    either (const $ return timeZero) return result
  where
    fileName = getStateFile feedUri
    timeZero = posixSecondsToUTCTime 0


-- | Write the last check time in the state file.
storeLastCheck :: (OptionsReader m, MonadBase IO m, MonadError ImmError m) => URI -> UTCTime -> m ()
storeLastCheck feedUri date = do
    directory <- getStateDirectory

    (file, stream) <- try $ openTempFile directory fileName
    io $ hPutStrLn stream (formatTime defaultTimeLocale "%c" date)
    io $ hClose stream
    try $ renameFile file (directory </> fileName)
  where
    fileName = getStateFile feedUri


forget :: (OptionsReader m, MonadBase IO m, MonadError ImmError m) => URI -> m ()
forget uri = do
    directory <- getStateDirectory
    try $ removeFile (directory </> getStateFile uri)
