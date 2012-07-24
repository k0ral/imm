module Imm.Mail where

-- {{{ Imports
import Imm.Feed
import Imm.Types
import Imm.Util

import Control.Monad

import qualified Data.Text.Lazy as T
import Data.Time
import Data.Time.RFC2822

import Text.Feed.Query
import Text.Feed.Types
-- }}}


bare :: Mail
bare = Mail {
    mCharset            = "utf-8",
    mContent            = T.pack "",
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
    mContent mail]
 

fromItem :: Settings -> TimeZone -> (Item, Feed) -> Mail
fromItem settings timeZone (item, feed) = bare {
    mDate       = maybe Nothing (Just . utcToZonedTime timeZone) . parseDate <=< getItemDate $ item,
    mFrom       = maybe (getFeedTitle feed) id $ getItemAuthor item,
    mSubject    = T.pack $ maybe "Untitled" id $ getItemTitle item,
    mContent    = buildMailBody item}
