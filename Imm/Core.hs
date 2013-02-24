{-# LANGUAGE TypeFamilies #-}
module Imm.Core where

-- {{{ Imports
import Imm.Config
import Imm.Database
import Imm.Error
import Imm.Feed (ImmFeed, FeedID)
import qualified Imm.Feed as Feed
import qualified Imm.Maildir as Maildir
import qualified Imm.Mail as Mail
import Imm.OPML as OPML
import Imm.Options (CliOptions(..), OptionsReader(..), log, logV)
import qualified Imm.Options as Options
import Imm.Util

import Control.Applicative
import Control.Conditional
import Control.Lens hiding((??))
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Base
import Control.Monad.Error hiding(forM_, mapM_)
import Control.Monad.Reader hiding(forM_, mapM_)
import Control.Monad.Trans.Control

import Data.Default
import Data.Foldable
-- import Data.Functor
import Data.Maybe
import Data.Time as T
import Data.Time.RFC2822
import Data.Time.RFC3339

import Prelude hiding(log, mapM_, sum)

import System.Directory
import System.Locale

import Text.Feed.Query as F
import Text.Feed.Types as F
-- }}}

-- {{{ Types
type FeedList = [(Config -> Config, FeedID)]

newtype I a = I { unIT :: ErrorT ImmError (ReaderT CliOptions (ReaderT Config IO)) a}
    deriving (Applicative, Functor, Monad, MonadBase IO, MonadError ImmError)

instance MonadBaseControl IO I where
    newtype StM I a  = StI { unStI :: StM (ErrorT ImmError (ReaderT CliOptions (ReaderT Config IO))) a }
    liftBaseWith f   = I . liftBaseWith $ \runInBase -> f $ liftM StI . runInBase . unIT
    restoreM         = I . restoreM . unStI


instance ConfigReader I where
    readConfig l = I $ (lift . lift) ask >>= return . view l

instance OptionsReader I where
    readOptions l = I $ lift ask >>= return . view l

runI :: CliOptions -> Config -> I a -> IO (Either ImmError a)
runI options config i = do
    (`runReaderT` config). (`runReaderT` options) . runErrorT $ unIT i

runI' :: CliOptions -> Config -> I () -> IO ()
runI' options config i = do
    result <- runI options config i
    either print return result
-- }}}


checkStateDirectory :: (OptionsReader m, MonadBase IO m, MonadError ImmError m) => m ()
checkStateDirectory = try . io . (createDirectoryIfMissing True) =<< Options.getStateDirectory


check :: (MonadBase IO m) => CliOptions -> FeedList -> m ()
check options feeds = io . forM_ feeds $ \(f, feedID) -> runI' options (f def) $ do
    log $ "Checking: " ++ show feedID
    Feed.check =<< Feed.download feedID


importOPML :: (MonadBase IO m) => m ()
importOPML = io $ mapM_ addFeeds =<< OPML.read <$> getContents


list :: CliOptions -> FeedList -> IO ()
list options = mapM_ (\(f, feedID) -> runI' options (f def) $ (io . putStrLn =<< Feed.showStatus feedID))


markAsRead :: CliOptions -> FeedList -> IO ()
markAsRead options = mapM_ (\(f, feedID) -> runI options (f def) $ checkStateDirectory >> Feed.markAsRead feedID)


markAsUnread :: CliOptions -> FeedList -> IO ()
markAsUnread options = mapM_ (\(f, feedID) -> runI options (f def) $ Feed.markAsUnread feedID)


update :: (MonadBase IO m) => CliOptions -> FeedList -> m ()
update options feeds = io . forM_ feeds $ \(f, feedID) -> do
    runI' options (f def) $ do
        log $ "Updating: " ++ show feedID
        checkStateDirectory
        updateFeed =<< Feed.download feedID

-- | Write mails for each new item, and update the last check time in state file.
updateFeed :: (Applicative m, ConfigReader m, MonadBase IO m, OptionsReader m, MonadError ImmError m) => ImmFeed -> m ()
updateFeed (uri, feed) = do
--    checkStateDirectory
    Maildir.create =<< readConfig maildir

    logV $ Feed.describe feed

    lastCheck <- getLastCheck uri
    results <- forM (feedItems feed) $ \item -> do
        (Right date) <- Feed.getDate item
        (date > lastCheck) ? (updateItem (item, feed) >> return 1) ?? return 0
    log $ "==> " ++ show (sum results) ++ " new item(s)"
    Feed.markAsRead uri


updateItem :: (Applicative m, ConfigReader m, MonadBase IO m, OptionsReader m, MonadError ImmError m) => (Item, Feed) -> m ()
updateItem (item, feed) = do
    logV $ Feed.describeItem item

    timeZone <- io getCurrentTimeZone
    dir <- readConfig maildir
    Maildir.add dir =<< Mail.build timeZone (item, feed)


instance Default Config where
    def = Config {
        _maildir       = "feeds",
        _dateParsers   = [
            return . zonedTimeToUTC <=< readRFC2822,
            return . zonedTimeToUTC <=< readRFC3339,
            T.parseTime defaultTimeLocale "%a, %d %b %G %T",
            T.parseTime defaultTimeLocale "%Y-%m-%d",
            T.parseTime defaultTimeLocale "%e %b %Y",
            T.parseTime defaultTimeLocale "%a, %e %b %Y %k:%M:%S %z",
            T.parseTime defaultTimeLocale "%a, %e %b %Y %T %Z"],
        _formatFrom    = \(item, feed) -> fromMaybe (getFeedTitle feed) $ getItemAuthor item,
        _formatSubject = \(item, _feed) -> fromMaybe "Untitled" $ getItemTitle item,
        _formatBody    = defaultBody,
        _decoder       = "UTF-8"
    }

defaultBody :: (Item, Feed) -> String
defaultBody (item, _feed) = "<p>" ++ link ++ "</p><p>" ++ (null content ? description ?? content) ++ "</p>"
  where
    link        = fromMaybe "No link found." $ getItemLink item
    content     = Feed.getItemContent item
    description = fromMaybe "No description." $ getItemDescription item
