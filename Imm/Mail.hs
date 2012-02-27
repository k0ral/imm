module Imm.Mail where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Control.Monad

import Data.Time.Clock.POSIX

import Text.Feed.Query
import Text.Feed.Types
-- }}}

                
defaultMail :: Mail
defaultMail = Mail {
    mCharset            = "utf-8",
    mContent            = "",
    mContentDisposition = "inline",
    mDate               = posixSecondsToUTCTime 0,
    mFrom               = "imm",
    mMIME               = "text/html",
    mSubject            = "Untitled",
    mReturnPath         = "<imm@noreply>"}

 
itemToMail :: Item -> Mail
itemToMail item = defaultMail {
    mDate       = maybe (posixSecondsToUTCTime 0) id . (stringToUTC <=< getItemDate) $ item,
    mFrom       = maybe "Anonymous" id $ getItemAuthor item,
    mSubject    = maybe "Untitled" id $ getItemTitle item,
    mContent    = maybe "Empty" id $ getItemDescription item}
