{-# LANGUAGE FlexibleContexts #-}
module Imm.Mail where

-- {{{ Imports
--import Imm.Feed
import Imm.Types
import Imm.Util

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Default
import qualified Data.Text.Lazy as T
import Data.Time
import Data.Time.RFC2822

import Text.Feed.Query as F
import Text.Feed.Types
-- }}}


instance Default Mail where
    def = Mail {
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
 
-- | Build mail from a given feed, using builders functions from 'Settings'.
build :: (Applicative m, MonadReader Settings m) => TimeZone -> (Item, Feed) -> m Mail
build timeZone (item, feed) = do
    from    <- asks mFromBuilder    <*> return (item, feed)
    subject <- asks mSubjectBuilder <*> return (item, feed)
    body    <- asks mBodyBuilder    <*> return (item, feed)
    return def {mDate = date, mFrom = from, mSubject = subject, mBody = body}
  where
    date = maybe Nothing (Just . utcToZonedTime timeZone) . parseDate <=< F.getItemDate $ item
