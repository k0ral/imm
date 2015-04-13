{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
module Imm.Core (
-- * Types
    FeedConfig,
    FeedList,
-- * Actions
    importOPML,
    check,
    showStatus,
    markAsRead,
    markAsUnread,
    update,
) where

-- {{{ Imports
import Imm.Config
import Imm.Database
import Imm.Error
import Imm.Feed (FeedParser(..))
import qualified Imm.Feed as Feed
import qualified Imm.HTTP as HTTP
import qualified Imm.Maildir as Maildir
import Imm.Mail (MailFormatter(..))
import qualified Imm.Mail as Mail
import Imm.OPML as OPML
import Imm.Util

import Control.Concurrent.Async
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_)
-- import Control.Monad.Reader hiding(forM_, mapM_)
import Control.Monad.Trans.Control

import Data.Foldable hiding(foldr)
import Data.Time as T

import Prelude hiding(log, mapM_, sum)

import System.Log.Logger

import Text.Feed.Query as F
import Text.Feed.Types as F
-- }}}

-- {{{ Types
type FeedConfig = (Config -> Config, FeedID)
type FeedList   = [FeedConfig]
-- }}}


importOPML :: (MonadBase IO m, MonadPlus m) => String -> m ()
importOPML = mapM_ addFeeds . OPML.read


check :: (MonadBaseControl IO m, FeedParser m, ConfigReader m, DatabaseReader m, HTTP.Decoder m, MonadError ImmError m) => FeedList -> m ()
check feeds = void . liftBaseWith $ \runInIO -> mapConcurrently (runInIO . checkFeed) feeds

checkFeed :: (MonadBase IO m, FeedParser m, ConfigReader m, DatabaseReader m, HTTP.Decoder m, MonadError ImmError m) => FeedConfig -> m ()
checkFeed (f, feedID) = localConfig f . localError "imm.core" $ Feed.download feedID >>= Feed.check


showStatus :: (MonadBase IO m, ConfigReader m, DatabaseReader m, MonadError ImmError m) => FeedConfig -> m ()
showStatus (f, feedID) = localConfig f . localError "imm.core" $ (io . noticeM "imm.core" =<< Feed.showStatus feedID)


markAsRead :: (MonadBase IO m, ConfigReader m, DatabaseState m, MonadError ImmError m) => FeedConfig -> m ()
markAsRead (f, feedID) = localConfig f . localError "imm.core" $ Feed.markAsRead feedID


markAsUnread :: (MonadBase IO m, ConfigReader m, DatabaseState m, MonadError ImmError m) => FeedConfig -> m ()
markAsUnread (f, feedID) = localConfig f . localError "imm.core" $ Feed.markAsUnread feedID


update :: (MonadBaseControl IO m, ConfigReader m, DatabaseState m, MonadError ImmError m, FeedParser m, MailFormatter m, HTTP.Decoder m) => FeedList -> m ()
update feeds = void . liftBaseWith $ \runInIO -> mapConcurrently (runInIO . updateFeed) feeds


-- | Write mails for each new item, and update the last check time in state file.
updateFeed :: (Applicative m, ConfigReader m, DatabaseState m, FeedParser m, MailFormatter m, MonadBase IO m, HTTP.Decoder m, MonadError ImmError m) => FeedConfig -> m ()
updateFeed (f, feedID) = localConfig f . localError "imm.core" $ do
    -- io . noticeM "imm.core" $ "Updating: " ++ show feedID
    (uri, feed) <- Feed.download feedID

    Maildir.create =<< readConfig maildir

    io . debugM "imm.core" $ Feed.describe feed

    lastCheck <- getLastCheck uri
    (results :: [Integer]) <- forM (feedItems feed) $ \item -> do
        date <- Feed.getDate item
        (date > lastCheck) ? (updateItem (item, feed) >> return 1) ?? return 0
    io . noticeM "imm.core" $ "==> " ++ show (sum results) ++ " new item(s) for <" ++ show feedID ++ ">"
    Feed.markAsRead uri


updateItem :: (Applicative m, ConfigReader m, FeedParser m, MailFormatter m, MonadBase IO m, MonadError ImmError m) => (Item, Feed) -> m ()
updateItem (item, feed) = do
    timeZone <- io getCurrentTimeZone
    dir <- readConfig maildir

    io . debugM "imm.core" $ "Adding following item to maildir [" ++ dir ++ "]:\n" ++ Feed.describeItem item
    Maildir.add dir =<< Mail.build timeZone (item, feed)
