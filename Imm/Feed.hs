module Imm.Feed where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Control.Error
--import Control.Exception

import Data.Time
import Data.Time.Clock.POSIX

import System.Directory
import System.FilePath
import System.IO
import System.Locale
-- }}}


getLastCheck :: Settings -> ImmFeed -> IO UTCTime
getLastCheck settings (uri, _feed) = do
    directory <- resolve $ mStateDirectory settings
    
    result <- runEitherT $ do
        content <- fmapLT OtherError . tryIO $ readFile (directory </> fileName)
        EitherT . return $ parseTime' content

    either 
        (\e -> print e >> return timeZero)
        return
        result
  where
    fileName = show uri >>= escapeFileName
    timeZero = posixSecondsToUTCTime $ 0 
    parseTime' string = note (ParseTimeError string) $ parseTime defaultTimeLocale "%c" string


storeLastCheck :: Settings -> ImmFeed -> UTCTime -> EitherT ImmError IO ()
storeLastCheck settings (uri, _) date = do
    directory <- io . resolve $ mStateDirectory settings
    let fileName = show uri >>= escapeFileName
    
    (file, stream) <- fmapLT OtherError . tryIO $ openTempFile directory fileName
    io $ hPutStrLn stream (formatTime defaultTimeLocale "%c" date)
    io $ hClose stream
    fmapLT OtherError . tryIO $ renameFile file (directory </> fileName)
