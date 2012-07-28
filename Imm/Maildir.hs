{-# LANGUAGE FlexibleContexts #-}
module Imm.Maildir where

-- {{{ Imports
import Imm.Mail
import Imm.Types
import Imm.Util

--import Control.Monad.IO.Class
import Control.Monad.Error

import Data.Functor
import qualified Data.Text.Lazy.IO as T
import Data.Time.Clock.POSIX

import Network.BSD

import System.Directory
import System.FilePath
import System.Random
-- }}}

init :: (MonadIO m, MonadError ImmError m) => PortableFilePath -> m ()
init directory = do
    dir <- io $ resolve directory
    try $ createDirectoryIfMissing True dir
    try $ createDirectoryIfMissing True (dir </> "cur")
    try $ createDirectoryIfMissing True (dir </> "new")
    try $ createDirectoryIfMissing True (dir </> "tmp")
    logVerbose $ "Maildir correctly created at: " ++ dir


add :: (MonadIO m, MonadError ImmError m) => PortableFilePath -> Mail -> m ()
add directory mail = do
    dir      <- resolve directory
    fileName <- io getUniqueName
    try $ T.writeFile (dir </> "new" </> fileName) (toText mail)


getUniqueName :: MonadIO m => m String 
getUniqueName = io $ do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)
    
    return . concat $ [time, ".", rand, ".", hostname]

