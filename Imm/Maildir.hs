module Imm.Maildir where

-- {{{ Imports
import Imm.Mail()
import Imm.Types
import Imm.Util

import Data.Time.Clock.POSIX

import Network.BSD

import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.IO.Error
import System.Random
-- }}}

init :: PortableFilePath -> IO Bool
init directory = do
    dir  <- resolve directory
    root <- try $ createDirectoryIfMissing True dir
    cur  <- try . createDirectoryIfMissing True . (dir </>) $ "cur"
    new  <- try . createDirectoryIfMissing True . (dir </>) $ "new"
    tmp  <- try . createDirectoryIfMissing True . (dir </>) $ "tmp"
    
    case (root, cur, new, tmp) of
        (Right _, Right _, Right _, Right _) -> do
            whenLoud . putStrLn . ("Maildir correctly created at: " ++) $ dir
            return True
        _                                    -> do
            whenNormal . putStrLn . ("Unable to initialize maildir at: " ++) $ dir
            return False

add :: PortableFilePath -> Mail -> IO ()
add directory mail = do
    dir      <- resolve directory
    fileName <- getUniqueName
    writeFile (dir </> "new" </> fileName) (show mail)


getUniqueName :: IO String    
getUniqueName = do
    time     <- getPOSIXTime >>= (return . show)
    hostname <- getHostName
    rand     <- (getStdRandom $ randomR (1,100000) :: IO Int) >>= (return . show)
    
    return $ time ++ "." ++ rand ++ "." ++ hostname

