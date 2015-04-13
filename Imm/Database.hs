{-# LANGUAGE OverlappingInstances, TemplateHaskell #-}
module Imm.Database (
    FeedID,
    DatabaseReader(..),
    DatabaseWriter(..),
    DatabaseState,
    FileDatabase,
    directory,
    getDataFile,
) where

-- {{{ Imports
import Imm.Error
import Imm.Util

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Error

import Data.Time hiding(parseTime)
import Data.Time.Clock.POSIX

import Network.URI

import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath
import System.Locale
import System.IO
import System.Log.Logger
-- }}}

-- {{{ Types
type FeedID  = URI

class DatabaseReader m where
    -- | Read the last check time in the state file.
    getLastCheck :: FeedID -> m UTCTime

instance (Error e, DatabaseReader m) => DatabaseReader (ErrorT e m) where
    getLastCheck = getLastCheck

class DatabaseWriter m where
    -- | Write the last update time in the data file.
    storeLastCheck :: FeedID -> UTCTime -> m ()
    -- | Remove state file as if no update was ever done.
    forget         :: FeedID -> m ()


type (DatabaseState m) = (DatabaseReader m, DatabaseWriter m)


data FileDatabase = FileDatabase {
    _directory   :: FilePath,
    _getDataFile :: FeedID -> FilePath
}

makeLenses ''FileDatabase

-- | A state file stores the last check time for a single feed, identified with its 'URI'.
instance Default (IO FileDatabase) where
    def = do
        dataDir         <- getUserConfigDir "imm" >/> "state"
        return FileDatabase {
            _directory = dataDir,
            _getDataFile = \feedUri -> case uriAuthority feedUri of
                Just auth -> toFileName =<< ((++ uriQuery feedUri) . (++ uriPath feedUri) . uriRegName $ auth)
                _         -> show feedUri >>= toFileName
                }

instance (MonadBase IO m) => DatabaseReader (ReaderT FileDatabase m) where
    getLastCheck feedUri = do
        dataDirectory  <- asks (view directory)
        dataFileGetter <- asks (view getDataFile)

        let dataFile = dataDirectory </> dataFileGetter feedUri
        io . debugM "imm.database" $ "Reading last check time from: " ++ dataFile

        result <- runErrorT $ do
            content <- try $ readFile dataFile
            parseTime content
        either (const $ io (debugM "imm.database" "Unable to read last update time.") >> return timeZero) return result
      where
        timeZero = posixSecondsToUTCTime 0

instance (MonadBase IO m, MonadError ImmError m) => DatabaseWriter (ReaderT FileDatabase m) where
    storeLastCheck feedUri date = do
        dataDirectory  <- asks (view directory)
        dataFileGetter <- asks (view getDataFile)

        let dataFile = dataFileGetter feedUri

        io . debugM "imm.database" $ "Storing last update time [" ++ show date ++ "] at <" ++ dataDirectory </> dataFile ++ ">"
        try . io . createDirectoryIfMissing True $ dataDirectory
        (file, stream) <- try $ openTempFile dataDirectory dataFile
        io $ hPutStrLn stream (formatTime defaultTimeLocale "%c" date)
        io $ hClose stream
        try $ renameFile file (dataDirectory </> dataFile)

    forget uri = do
        dataDirectory  <- asks (view directory)
        dataFileGetter <- asks (view getDataFile)

        let dataFile = dataDirectory </> dataFileGetter uri
        io . debugM "imm.database" $ "Removing data file <" ++ dataFile ++ ">"
        try $ removeFile dataFile
-- }}}

-- | Remove forbidden characters in a filename.
toFileName :: Char -> String
toFileName '/' = "."
toFileName '?' = "."
toFileName x   = [x]
