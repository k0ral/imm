{-# LANGUAGE DeriveDataTypeable #-}
module Imm.Types where

-- {{{ Imports
--import Network.URI

import Data.Time
import Data.Time.RFC2822

import System.Console.CmdArgs

--import Text.Feed.Types
-- }}}

data CliOptions = CliOptions {
    mParameter    :: Maybe String,
    mMasterBinary :: Maybe String} 
  deriving (Data, Typeable, Show, Eq)

-- | 
data Settings = Settings {
    mStateDirectory  :: PortableFilePath,
    mError           :: Maybe String}

type FeedGroup = (FeedSettings, [String]) 

data FeedSettings = FeedSettings {
    mMailDirectory  :: PortableFilePath}

{-data ImmFeed = ImmFeed {
    mURI  :: URI,
    mFeed :: Feed
}-}

data Mail = Mail {
    mReturnPath         :: String,
    mDate               :: Maybe ZonedTime,
    mFrom               :: String,
    mSubject            :: String,
    mMIME               :: String,
    mCharset            :: String,
    mContentDisposition :: String,
    mContent            :: String}

instance Show Mail where 
    show mail = unlines [
        "Return-Path: " ++ mReturnPath mail,
        maybe "" (("Date: " ++) . showRFC2822) . mDate $ mail,
        "From: " ++ mFrom mail,
        "Subject: " ++ mSubject mail,
        "Content-Type: " ++ mMIME mail ++ "; charset=" ++ mCharset mail,
        "Content-Disposition: " ++ mContentDisposition mail,
        "",
        mContent mail]

-- | Set of reference directories, typically used to build FilePath-s
data RefDirs = RefDirs {
    mHome          :: FilePath,        -- ^ Home directory
    mTemporary     :: FilePath,        -- ^ Temporary files directory
    mConfiguration :: FilePath,        -- ^ Configuration directory
    mData          :: FilePath}        -- ^ Data directory

type PortableFilePath = RefDirs -> FilePath

