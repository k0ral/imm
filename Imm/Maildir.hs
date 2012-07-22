module Imm.Maildir where

-- {{{ Imports
import Imm.Mail
import Imm.Types
import Imm.Util

import Control.Error
--import Control.Exception

import Data.Functor
import qualified Data.Text.Lazy.IO as T
import Data.Time.Clock.POSIX

import Network.BSD

import System.Directory
import System.FilePath
--import System.IO.Error (ioeGetErrorString)
import System.Random
-- }}}

init :: PortableFilePath -> EitherT String IO ()
init directory = do
    dir <- io $ resolve directory
    tryIO $ createDirectoryIfMissing True dir
    tryIO $ createDirectoryIfMissing True (dir </> "cur")
    tryIO $ createDirectoryIfMissing True (dir </> "new")
    tryIO $ createDirectoryIfMissing True (dir </> "tmp")
    
    io . logVerbose $ "Maildir correctly created at: " ++ dir


add :: PortableFilePath -> Mail -> Script ()
add directory mail = do
    dir      <- io $ resolve directory
    fileName <- io getUniqueName
    tryIO $ T.writeFile (dir </> "new" </> fileName) (toText mail)


getUniqueName :: IO String 
getUniqueName = do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)
    
    return . concat $ [time, ".", rand, ".", hostname]

