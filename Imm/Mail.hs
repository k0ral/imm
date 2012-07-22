module Imm.Mail where

-- {{{ Imports
import Imm.Types
import Imm.Util

import Control.Monad

import qualified Data.Text.Lazy as T
import Data.Time
import Data.Time.RFC2822

import Text.Feed.Query
import Text.Feed.Types
import Text.Atom.Feed   as Atom
import Text.XML.Light.Proc
-- }}}


defaultMail :: Mail
defaultMail = Mail {
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
 

itemToMail :: TimeZone -> Item -> Mail
itemToMail timeZone item = defaultMail {
    mDate       = maybe Nothing (Just . utcToZonedTime timeZone) . parseDate <=< getItemDate $ item,
    mFrom       = maybe "Anonymous" id $ getItemAuthor item,
    mSubject    = T.pack $ maybe "Untitled" id $ getItemTitle item,
    mContent    = buildMailBody item}

buildMailBody :: Item -> T.Text
buildMailBody item = 
    T.unlines $ map ((><) item) [T.pack . getItemLinkNM, getItemContent]

getItemLinkNM :: Item -> String 
getItemLinkNM item = maybe "No link found" paragraphy  $ getItemLink item

-- ce "magic operator" semble pas défini dans les libs haskell -> WTF ?
(><) :: a -> (a -> b) -> b
(><) a b = b a

paragraphy :: String -> String
paragraphy s = "<p>"++s++"</p>"


getItemContent :: Item -> T.Text
getItemContent (AtomItem e) = T.pack . maybe "No content" extractHtml . Atom.entryContent $ e
getItemContent item = T.pack . maybe "Empty" id . getItemDescription $ item


extractHtml :: EntryContent -> String
extractHtml (HTMLContent c) = c
extractHtml (XHTMLContent c) = strContent c
extractHtml (TextContent t) = t
extractHtml (MixedContent a b)= show a ++ show b
extractHtml (ExternalContent mediaType uri) = show mediaType ++ show uri
