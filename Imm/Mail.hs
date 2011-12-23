module Imm.Mail where

import Imm.Types

import Data.Time.LocalTime


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