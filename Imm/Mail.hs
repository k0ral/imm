{-# LANGUAGE TemplateHaskell #-}
module Imm.Mail where

-- {{{ Imports
import Imm.Config
import Imm.Feed

import Control.Applicative
import Control.Lens hiding(from)

import Data.Default
import Data.Time
import Data.Time.RFC2822

import Text.Feed.Types
-- }}}


data Mail = Mail {
    _returnPath         :: String,
    _date               :: Maybe ZonedTime,
    _from               :: String,
    _subject            :: String,
    _mime               :: String,
    _charset            :: String,
    _contentDisposition :: String,
    _body               :: String
}

makeLenses ''Mail


instance Default Mail where
    def = Mail {
        _charset            = "utf-8",
        _body               = empty,
        _contentDisposition = "inline",
        _date               = Nothing,
        _from               = "imm",
        _mime               = "text/html",
        _subject            = "Untitled",
        _returnPath         = "<imm@noreply>"}


instance Show Mail where
    show mail = unlines [
        "Return-Path: " ++ view returnPath mail,
        maybe "" (("Date: " ++) . showRFC2822) . view date $ mail,
        "From: " ++ view from mail,
        "Subject: " ++ view subject mail,
        "Content-Type: " ++ view mime mail ++ "; charset=" ++ view charset mail,
        "Content-Disposition: " ++ view contentDisposition mail,
        "",
        view body mail]


-- | Build mail from a given feed, using builders functions from 'Settings'.
build :: (Applicative m, ConfigReader m, Monad m) => TimeZone -> (Item, Feed) -> m Mail
build timeZone (item, feed) = do
    from'    <- readConfig formatFrom    <*> return (item, feed)
    subject' <- readConfig formatSubject <*> return (item, feed)
    body'    <- readConfig formatBody    <*> return (item, feed)
    date'    <- either (const Nothing) (Just . utcToZonedTime timeZone) <$> getDate item
    return . set date date' . set from from' . set subject subject' . set body body' $ def
