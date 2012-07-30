{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imm.Types where

-- {{{ Imports
import Control.Monad.Error

import Data.Text.Encoding.Error
import Data.Text.Lazy as T hiding(unlines)
import Data.Time

import Network.URI
import Network.Stream

import System.Console.CmdArgs
import System.IO.Error

import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- {{{ Error handling
-- | Errors that can be returned by an Imm process
data ImmError = 
    OtherError         String
  | HTTPError          String
  | UnicodeError       UnicodeException
  | ParseUriError      String
  | ParseTimeError     String
  | ParseItemDateError Item
  | ParseFeedError     String
  | CE                 ConnError
  | IOE                IOError

instance Show ImmError where
    show (OtherError e)            = e
    show (HTTPError e)             = e
    show (UnicodeError (DecodeError e _)) = e
    show (UnicodeError (EncodeError e _)) = e
    show (ParseUriError raw)       = "Cannot parse URI: " ++ raw
    show (ParseItemDateError item) = unlines [
        "Cannot parse date from item: ",
        "    title: "       ++ (show $ getItemTitle item),
        "    link:"         ++ (show $ getItemLink item),
        "    publish date:" ++ (show $ getItemPublishDate item),
        "    date:"         ++ (show $ getItemDate item)]
    show (ParseTimeError raw)      = "Cannot parse time: " ++ raw
    show (ParseFeedError raw)      = "Cannot parse feed: " ++ raw
    show (CE e)                    = show e
    show (IOE e)                   = ioeGetLocation e ++ " " ++ maybe "" id (ioeGetFileName e) ++ " " ++ ioeGetErrorString e

instance Error ImmError where
    strMsg x = OtherError x
-- }}}

-- {{{ Settings type
data CliOptions = CliOptions {
    mCheck        :: Bool,
    mList         :: Bool,
    mDenyReconf   :: Bool,         -- ^ Do not recompile configuration even if it has changed
    mMasterBinary :: Maybe String
} deriving (Data, Typeable, Show, Eq)

-- | Set of settings for imm
data Settings = Settings {
    mStateDirectory :: PortableFilePath,
    mMaildir        :: PortableFilePath,
    mFromBuilder    :: (Item, Feed) -> String,
    mSubjectBuilder :: (Item, Feed) -> Text,
    mBodyBuilder    :: (Item, Feed) -> Text   -- ^ sic!
}

-- | 
type CustomSettings = Settings -> Settings
-- }}}

-- {{{ Feed types
type FeedList  = [(CustomSettings, String)]
type ImmFeed   = (URI, Feed)
-- }}}

data Mail = Mail {
    mReturnPath         :: String,
    mDate               :: Maybe ZonedTime,
    mFrom               :: String,
    mSubject            :: Text,
    mMIME               :: String,
    mCharset            :: String,
    mContentDisposition :: String,
    mBody               :: Text
}

-- {{{ Generic file paths
-- | Set of reference directories, typically used to build FilePath-s
data RefDirs = RefDirs {
    mHome          :: FilePath,        -- ^ Home directory
    mTemporary     :: FilePath,        -- ^ Temporary files directory
    mConfiguration :: FilePath,        -- ^ Configuration directory
    mData          :: FilePath         -- ^ Data directory
}

type PortableFilePath = RefDirs -> FilePath
-- }}}
