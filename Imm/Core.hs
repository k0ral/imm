{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Imm.Core (
-- * Types
    FeedConfig,
    FeedList,
-- * Actions
    dispatch,
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
import Imm.Maildir (MaildirWriter(..))
import qualified Imm.Maildir as Maildir
import Imm.Mail (MailFormatter(..))
import qualified Imm.Mail as Mail
import Imm.OPML as OPML
import Imm.Util

import Control.Concurrent.Async
import Control.Monad hiding(forM_, mapM_)
import Control.Monad.Error hiding(forM_, mapM_)
-- import Control.Monad.Reader hiding(forM_, mapM_)

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


dispatch :: (Config -> Config) -> Feed.Action -> FeedList -> IO ()
dispatch baseConfig Feed.Check        feeds = void $ mapConcurrently (check baseConfig) feeds
dispatch baseConfig Feed.ShowStatus   feeds = mapM_ (showStatus baseConfig) feeds
dispatch baseConfig Feed.MarkAsRead   feeds = mapM_ (markAsRead baseConfig) feeds
dispatch baseConfig Feed.MarkAsUnread feeds = mapM_ (markAsUnread baseConfig) feeds
dispatch baseConfig Feed.Update       feeds = void $ mapConcurrently (update baseConfig) feeds


importOPML :: (MonadBase IO m, MonadPlus m) => String -> m ()
importOPML = mapM_ addFeeds . OPML.read


check :: (Config -> Config) -> FeedConfig -> IO ()
check baseConfig (f, feedID) = withError "imm.core". withConfig (f . baseConfig) $ Feed.download feedID >>= Feed.check


showStatus :: (Config -> Config) -> FeedConfig -> IO ()
showStatus baseConfig (f, feedID) = withConfig (f . baseConfig) (io . noticeM "imm.core" =<< Feed.showStatus feedID)


markAsRead :: (Config -> Config) -> FeedConfig -> IO ()
markAsRead baseConfig (f, feedID) = withError "imm.core" . withConfig (f . baseConfig) $ Feed.markAsRead feedID


markAsUnread :: (Config -> Config) -> FeedConfig -> IO ()
markAsUnread baseConfig (f, feedID) = withError "imm.core" . withConfig (f . baseConfig) $ Feed.markAsUnread feedID


-- | Write mails for each new item, and update the last check time in state file.
update :: (Config -> Config) -> FeedConfig -> IO ()
update baseConfig (f, feedID) = withError "imm.core" . withConfig (f . baseConfig) $ do
    -- io . noticeM "imm.core" $ "Updating: " ++ show feedID
    (uri, feed) <- Feed.download feedID

    Maildir.init

    io . debugM "imm.core" $ Feed.describe feed

    lastCheck <- getLastCheck uri
    (results :: [Integer]) <- forM (feedItems feed) $ \item -> do
        date <- Feed.getDate item
        (date > lastCheck) ? (updateItem (item, feed) >> return 1) ?? return 0
    io . noticeM "imm.core" $ "==> " ++ show (sum results) ++ " new item(s) for <" ++ show feedID ++ ">"
    Feed.markAsRead uri


updateItem :: (Applicative m, FeedParser m, MaildirWriter m, MailFormatter m, MonadBase IO m, MonadError ImmError m) => (Item, Feed) -> m ()
updateItem (item, feed) = do
    timeZone <- io getCurrentTimeZone
    Maildir.write =<< Mail.build timeZone (item, feed)
