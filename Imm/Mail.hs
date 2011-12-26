module Imm.Mail where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Text.Feed.Query
import Text.Feed.Types
-- }}}

instance Show Mail where 
    show mail = unlines [
        "Return-Path: " ++ mReturnPath mail,
        "Date: " ++ (maybe "" show (mDate mail)),
        "From: " ++ mFrom mail,
        "Subject: " ++ mSubject mail,
        "Content-Type: " ++ mMIME mail ++ "; charset=" ++ mCharset mail,
        "Content-Disposition: " ++ mContentDisposition mail,
        "",
        mContent mail
        ]
                
                
defaultMail :: Mail
defaultMail = Mail {
    mMIME = "text/html"
}

 
itemToMail :: Item -> Mail
itemToMail item = defaultMail {
    mReturnPath = "<noreply@anonymous.net>",
    mDate       = stringToUTC $ (maybe "" id $ getItemDate item),
    mFrom       = maybe "Anonymous" id $ getItemAuthor item,
    mSubject    = maybe "Untitled" id $ getItemTitle item,
    mCharset    = "utf-8",
    mContentDisposition = "inline",
    mContent = maybe "Empty" id $ getItemDescription item
}