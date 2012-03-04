module Imm.Mail where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Control.Monad

import Data.Time

import Text.Feed.Query
import Text.Feed.Types
import Text.Atom.Feed   as Atom
-- }}}


defaultMail :: Mail
defaultMail = Mail {
    mCharset            = "utf-8",
    mContent            = "",
    mContentDisposition = "inline",
    mDate               = Nothing,
    mFrom               = "imm",
    mMIME               = "text/html",
    mSubject            = "Untitled",
    mReturnPath         = "<imm@noreply>"}

 
itemToMail :: TimeZone -> Item -> Mail
itemToMail timeZone item = defaultMail {
    mDate       = maybe Nothing (Just . utcToZonedTime timeZone) . parseDate <=< getItemDate $ item,
    mFrom       = maybe "Anonymous" id $ getItemAuthor item,
    mSubject    = maybe "Untitled" id $ getItemTitle item,
    mContent    = getItemContent item}


getItemContent :: Item -> String
getItemContent (AtomItem e) = maybe "No content" (show . extractHtml) ( Atom.entryContent e)
getItemContent item = maybe "Empty" id $ getItemDescription item


extractHtml :: EntryContent -> String
extractHtml (HTMLContent c) = c
extractHtml (XHTMLContent c) = show c
extractHtml (TextContent t) = t
extractHtml (MixedContent a b)=show a ++ show b
extractHtml (ExternalContent mediaType uri) = show mediaType ++ show uri
