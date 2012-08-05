{-# LANGUAGE FlexibleContexts #-}
-- | Utilities to manipulate maildirs.
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
--import System.FilePath
import System.Random
-- }}}

init :: (MonadIO m, MonadError ImmError m) => IO FilePath -> m ()
init directory = do
    try $ createDirectoryIfMissing True =<< directory
    try $ createDirectoryIfMissing True =<< (directory >/> "cur")
    try $ createDirectoryIfMissing True =<< (directory >/> "new")
    try $ createDirectoryIfMissing True =<< (directory >/> "tmp")


add :: (MonadIO m, MonadError ImmError m) => IO FilePath -> Mail -> m ()
add directory mail = do
    fileName <- io getUniqueName
    dir      <- directory >/> "new" >/> fileName 
    try $ T.writeFile dir (toText mail)


getUniqueName :: MonadIO m => m String 
getUniqueName = io $ do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)
    
    return . concat $ [time, ".", rand, ".", hostname]

