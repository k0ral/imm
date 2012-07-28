{-# LANGUAGE FlexibleContexts #-}
module Imm.Mail where

-- {{{ Imports
--import Imm.Feed
import Imm.Types
import Imm.Util

import Control.Monad
import Control.Monad.Reader

import qualified Data.Text.Lazy as T
import Data.Time
import Data.Time.RFC2822

import Text.Feed.Query as F
import Text.Feed.Types
-- }}}


bare :: Mail
bare = Mail {
    mCharset            = "utf-8",
    mBody               = T.pack "",
    mContentDisposition = "inline",
    mDate               = Nothing,
    mFrom               = "imm",
    mMIME               = "text/html",
    mSubject            = T.pack "Untitled",
    mReturnPath         = "<imm@noreply>"}


toText :: Mail -> T.Text
toText mail = T.unlines [
    T.pack $ "Return-Path: " ++ mReturnPath mail,
    T.pack $ maybe "" (("Date: " ++) . showRFC2822) . mDate $ mail,
    T.pack $ "From: " ++ mFrom mail,
    T.concat [T.pack "Subject: ", mSubject mail],
    T.pack $ "Content-Type: " ++ mMIME mail ++ "; charset=" ++ mCharset mail,
    T.pack $ "Content-Disposition: " ++ mContentDisposition mail,
    T.pack "",
    mBody mail]
 

build :: MonadReader Settings m => TimeZone -> (Item, Feed) -> m Mail
build timeZone (item, feed) = do
    from    <- buildFrom    (item, feed)
    subject <- buildSubject (item, feed)
    body    <- buildBody    (item, feed)
    return bare {mDate = date, mFrom = from, mSubject = subject, mBody = body}
  where
    date = maybe Nothing (Just . utcToZonedTime timeZone) . parseDate <=< F.getItemDate $ item


buildFrom :: MonadReader Settings m => (Item, Feed) -> m String
buildFrom (item, feed) = do
    builder <- asks mFromBuilder
    return $ builder (item, feed)


buildSubject :: MonadReader Settings m => (Item, Feed) -> m T.Text
buildSubject (item, feed) = do
    builder <- asks mSubjectBuilder
    return $ builder (item, feed)


buildBody :: MonadReader Settings m => (Item, Feed) -> m T.Text
buildBody (item, feed) = do
    builder <- asks mBodyBuilder
    return $ builder (item, feed)
