module Imm.Maildir where

-- {{{ Imports
import Imm.Mail()
import Imm.Types

import Data.Time.Clock.POSIX

import Network.BSD

import System.Directory
import System.FilePath
import System.IO.Error
import System.Random
-- }}}

init :: FilePath -> IO Bool
init directory = do
    root <- try $ createDirectoryIfMissing True directory
    cur  <- try . createDirectoryIfMissing True . (directory </>) $ "cur"
    new  <- try . createDirectoryIfMissing True . (directory </>) $ "new"
    tmp  <- try . createDirectoryIfMissing True . (directory </>) $ "tmp"
    
    case (root, cur, new, tmp) of
        (Right _, Right _, Right _, Right _) -> return True
        _                                    -> return False

add :: FilePath -> Mail -> IO ()
add directory mail = do
    fileName <- getUniqueName
    writeFile (directory </> "new" </> fileName) (show mail)


getUniqueName :: IO String    
getUniqueName = do
    time     <- getPOSIXTime >>= (return . show)
    hostname <- getHostName
    rand     <- (getStdRandom $ randomR (1,100000) :: IO Int) >>= (return . show)
    
    return $ time ++ "." ++ rand ++ "." ++ hostname

