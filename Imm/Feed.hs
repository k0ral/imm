{-# LANGUAGE FlexibleContexts, RankNTypes, KindSignatures #-}
module Imm.Feed where

-- {{{ Imports
import qualified Imm.HTTP as HTTP
import qualified Imm.Mail as Mail
import qualified Imm.Maildir as Maildir
import Imm.Types
import Imm.Util

import Control.Conditional hiding(when)
import Control.Monad.Error
import Control.Monad.Reader hiding(when)

--import Data.Functor
import qualified Data.Text.Lazy as T
import Data.Time hiding(parseTime)
import Data.Time.Clock.POSIX

import Network.URI as N

import System.Directory
--import System.FilePath
import System.IO
import System.Locale

import qualified Text.Atom.Feed as Atom
import Text.Feed.Import as F
import Text.Feed.Query as F
import Text.Feed.Types as F
import Text.XML.Light.Proc
-- }}}

-- {{{ Util
getStateFile :: URI -> FilePath
getStateFile feedUri@URI{ uriAuthority = Just auth } = toFileName =<< ((++ (uriQuery feedUri)) . (++ (uriPath feedUri)) . uriRegName $ auth)
getStateFile feedUri = show feedUri >>= toFileName

toFileName :: Char -> String
toFileName '/' = "."
toFileName '?' = "."
toFileName x = [x]
-- }}}

-- | Monad-agnostic version of 'Text.Feed.Import.parseFeedString'
parse :: MonadError ImmError m => String -> m Feed
parse x = maybe (throwError $ ParseFeedError x) return $ parseFeedString x

-- | 
printStatus :: (MonadReader Settings m, MonadIO m) => String -> m ()
printStatus feedUri = do
    prefix <- case N.parseURI feedUri of
        Just uri -> do
          lastCheck <- getLastCheck uri
          return $ (lastCheck == posixSecondsToUTCTime 0) ? "[NEW] " ?? ("[Last update: "++ show lastCheck ++ "]")
        _ -> return "[Not an URI]"
    io . putStrLn $ prefix ++ " " ++ feedUri


getLastCheck :: (MonadReader Settings m, MonadIO m) => URI -> m UTCTime
getLastCheck feedUri = do
    directory <- asks mStateDirectory
    result    <- runErrorT $ do
        content <- try $ readFile =<< (directory >/> fileName)
        parseTime content
        
    either (const $ return timeZero) return result
  where
    fileName = getStateFile feedUri
    timeZero = posixSecondsToUTCTime 0 


storeLastCheck :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => URI -> UTCTime -> m ()
storeLastCheck feedUri date = do
    directory <- asks mStateDirectory
    
    (file, stream) <- try $ (`openTempFile` fileName) =<< directory
    io $ hPutStrLn stream (formatTime defaultTimeLocale "%c" date)
    io $ hClose stream
    try $ renameFile file =<< (directory >/> fileName)
  where
    fileName = getStateFile feedUri

download :: (MonadIO m, MonadError ImmError m) => URI -> m ImmFeed
download uri = do
    feed <- parse . T.unpack =<< decode =<< HTTP.getRaw uri
    return (uri, feed)

-- | Create mails for each new item
update :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => ImmFeed -> m ()
update (uri, feed) = do
    logNormal $ "Updating feed " ++ show uri
--    checkStateDirectory
    Maildir.init =<< asks mMaildir

    logVerbose $ unlines [
        "Title:  " ++ (getFeedTitle feed),
        "Author: " ++ (maybe "No author" id $ getFeedAuthor feed),
        "Home:   " ++ (maybe "No home"   id $ getFeedHome feed)]
    
    lastCheck <- getLastCheck uri
    forM_ (feedItems feed) $ \item -> 
      do
        date <- getDate item
        when (date > lastCheck) $ updateItem (item, feed)
      `catchError` (io . print)

    markAsRead uri

updateItem :: (MonadReader Settings m, MonadIO m, MonadError ImmError m) => (Item, Feed) -> m ()
updateItem (item, feed) = do
    date <- getDate item
    logVerbose $ unlines [
            "   Item author: " ++ (maybe "<empty>" id $ getItemAuthor item),
            "   Item title:  " ++ (maybe "<empty>" id $ getItemTitle item),
            "   Item URI:    " ++ (maybe "<empty>" id $ getItemLink  item),
            -- "   Item Body:   " ++ (Imm.Mail.getItemContent  item),
            "   Item date:   " ++ show date]
    
    timeZone <- io getCurrentTimeZone
    dir      <- asks mMaildir
    Maildir.add dir =<< Mail.build timeZone (item, feed)


markAsRead :: forall (m :: * -> *) . (MonadIO m, MonadError ImmError m, MonadReader Settings m) => URI -> m ()
markAsRead uri = io getCurrentTime >>= storeLastCheck uri >> (logVerbose $ "Feed " ++ show uri ++ " marked as read.")

markAsUnread :: forall (m :: * -> *) . (MonadIO m, MonadError ImmError m, MonadReader Settings m) => URI -> m ()
markAsUnread uri = do
    directory <- asks mStateDirectory
    try $ removeFile =<< directory >/> (getStateFile uri)
    logVerbose $ "Feed " ++ show uri ++ " marked as unread."
    

-- {{{ Item utilities
getItemLinkNM :: Item -> String 
getItemLinkNM item = maybe "No link found" paragraphy  $ getItemLink item


getItemContent :: Item -> T.Text
getItemContent (AtomItem e) = T.pack . maybe "No content" extractHtml . Atom.entryContent $ e
getItemContent item = T.pack . maybe "Empty" id . getItemDescription $ item

getDate :: MonadError ImmError m => Item -> m UTCTime
getDate x = maybe (throwError $ ParseItemDateError x) return $ parseDate =<< F.getItemDate x
-- }}}


extractHtml :: Atom.EntryContent -> String
extractHtml (Atom.HTMLContent c) = c
extractHtml (Atom.XHTMLContent c) = strContent c
extractHtml (Atom.TextContent t) = t
extractHtml (Atom.MixedContent a b) = show a ++ show b
extractHtml (Atom.ExternalContent mediaType uri) = show mediaType ++ show uri


paragraphy :: String -> String
paragraphy s = "<p>"++s++"</p>"

