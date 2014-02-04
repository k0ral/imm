{-# LANGUAGE TemplateHaskell #-}
module Imm.Mail where

-- {{{ Imports
import Imm.Config
import Imm.Feed

import Control.Applicative
import Control.Lens hiding(from, parts)

import Data.Default
import Data.List
import Data.Time
import Data.Time.RFC2822

import Text.Feed.Types
-- }}}


data Mail = Mail {
    _returnPath         :: String,
    _date               :: Maybe ZonedTime,
    _from               :: String,
    _subject            :: String,
    _parts              :: [MultiPart],
    _separator          :: String,
    _introHeader        :: String
}

makeLenses ''Mail


instance Default Mail where
    def = Mail {
        _parts              = [],
        _separator          = "",
        _introHeader        = "",
        _date               = Nothing,
        _from               = "imm",
        _subject            = "Untitled",
        _returnPath         = "<imm@noreply>"}


instance Show Mail where
    show mail = unlines ([
        "Return-Path: " ++ view returnPath mail,
        maybe "" (("Date: " ++) . showRFC2822) . view date $ mail,
        "From: " ++ view from mail,
        "Subject: " ++ view subject mail] ++
        toText mail)

toText :: Mail -> [String]
toText mail = 
  case view parts mail of
  [simpleBody] -> [mpToText simpleBody]
  _            -> [view introHeader mail, "" {- neede by rfc2046-}, toText' mail]

toText' :: Mail -> String
toText' mail = (unlines . ls) (view parts mail)
  where b = "--" ++ (view separator mail)
        ls mps = [b] ++ (intersperse b (map mpToText mps)) ++ [b ++ "--"]

mpToText :: MultiPart -> String
mpToText mp = unlines (
  (view contentHeaders mp) ++ [empty, view content mp])

-- | Build mail from a given feed, using builders functions from 'Settings'.
build :: (Applicative m, ConfigReader m, Monad m) => TimeZone -> (Item, Feed) -> m Mail
build timeZone (item, feed) = do
    from'    <- readConfig formatFrom    <*> return (item, feed)
    subject' <- readConfig formatSubject <*> return (item, feed)
    parts'   <- readConfig formatParts   <*> return (item, feed)
    date'    <- either (const Nothing) (Just . utcToZonedTime timeZone) <$> getDate item
    separator' <- readConfig partsSeparator
    introHeader' <- readConfig multiPartsHeader <*> return separator'
    return . set date date' . set from from' . set subject subject' . set introHeader introHeader'. set separator separator' . set parts parts' $ def
