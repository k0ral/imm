module Imm.Maildir where

-- {{{ Imports
import Imm.Error
import Imm.Mail
import Imm.Util

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX

import Network.BSD

import System.Directory
import System.FilePath
import System.Log.Logger
import System.Random
-- }}}

type Maildir = FilePath

class MaildirWriter m where
    -- | Build a maildir with subdirectories cur, new and tmp.
    init :: m ()
    -- | Add a mail to the maildir
    write  :: Mail -> m ()

instance (MonadBase IO m, MonadError ImmError m) => MaildirWriter (ReaderT Maildir m) where
    init = do
        theMaildir <- ask
        io . debugM "imm.maildir" $ "Creating maildir [" ++ theMaildir ++ "]"
        try . mapM_ (createDirectoryIfMissing True) $
            theMaildir:
            (theMaildir </> "cur"):
            (theMaildir </> "new"):
            (theMaildir </> "tmp"):[]
    write mail = do
        fileName   <- io getUniqueName
        theMaildir <- ask
        io . debugM "imm.maildir" $ "Writing new mail in maildir [" ++ theMaildir ++ "]"
        try $ T.writeFile (theMaildir </> "new" </> fileName) (TL.pack $ show mail)

-- | Return an allegedly unique filename; useful to add new mail files in a maildir.
getUniqueName :: MonadBase IO m => m String
getUniqueName = io $ do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)

    return . concat $ time:".":rand:".":hostname:[]
