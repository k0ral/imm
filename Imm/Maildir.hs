module Imm.Maildir where

-- {{{ Imports
import Imm.Mail()
import Imm.Types
import Imm.Util

--import Control.Monad
import Control.Exception

import Data.Functor
import Data.Time.Clock.POSIX

import Network.BSD

import System.Directory
import System.FilePath
import System.IO.Error (ioeGetErrorString)
import System.Random
-- }}}

init :: PortableFilePath -> IO Bool
init directory = do
    dir  <- resolve directory
    root <- try $ createDirectoryIfMissing True dir
    cur  <- try $ createDirectoryIfMissing True (dir </> "cur")
    new  <- try $ createDirectoryIfMissing True (dir </> "new")
    tmp  <- try $ createDirectoryIfMissing True (dir </> "tmp")
    
    either
        (\e -> do 
          logNormal $ concat ["Unable to initialize maildir at ", dir, " : ", ioeGetErrorString e]
          return False)
        (const $ logVerbose ("Maildir correctly created at: " ++ dir) >> return True)
        $ sequence [root, cur, new, tmp]

add :: PortableFilePath -> Mail -> IO ()
add directory mail = do
    dir      <- resolve directory
    fileName <- getUniqueName
    writeFile (dir </> "new" </> fileName) (show mail)


getUniqueName :: IO String 
getUniqueName = do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)
    
    return . concat $ [time, ".", rand, ".", hostname]

