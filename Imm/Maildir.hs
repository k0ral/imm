module Imm.Maildir where

-- {{{ Imports
import Imm.Error
import Imm.Mail
import Imm.Util

import Control.Monad.Base
import Control.Monad.Error

import Data.Functor
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX

import Network.BSD

import System.Directory
import System.FilePath
import System.Random
-- }}}

-- | Build a maildir with subdirectories cur, new and tmp.
create :: (MonadBase IO m, MonadError ImmError m) => FilePath -> m ()
create directory = do
    try $ createDirectoryIfMissing True directory
    try $ createDirectoryIfMissing True (directory </> "cur")
    try $ createDirectoryIfMissing True (directory </> "new")
    try $ createDirectoryIfMissing True (directory </> "tmp")

-- | Add a mail to the maildir
add :: (MonadBase IO m, MonadError ImmError m) => FilePath -> Mail -> m ()
add directory mail = do
    fileName <- io getUniqueName
    try $ T.writeFile (directory </> "new" </> fileName) (TL.pack $ show mail)

-- | Return an allegedly unique filename; useful to add new mail files in a maildir.
getUniqueName :: MonadBase IO m => m String
getUniqueName = io $ do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)

    return . concat $ [time, ".", rand, ".", hostname]
