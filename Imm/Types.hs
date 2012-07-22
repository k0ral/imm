{-# LANGUAGE DeriveDataTypeable #-}
module Imm.Types where

-- {{{ Imports
import qualified Data.Text.Lazy as T
import Data.Time

import Network.URI
import Network.Stream

import System.Console.CmdArgs

import Text.Feed.Types
-- }}}

-- {{{ Error handling
-- | Errors that can be returned by an Imm process
data ImmError = OtherError String | ParseUriError String | ParseTimeError String | ParseItemDateError Item | ParseFeedError String | CE ConnError

instance Show ImmError where
    show (OtherError e)            = show e
    show (ParseUriError raw)       = "Cannot parse URI: " ++ raw
    show (ParseItemDateError item) = "Cannot parse date from item: " ++ show item
    show (ParseTimeError raw)      = "Cannot parse time: " ++ raw
    show (ParseFeedError raw)      = "Cannot parse feed: " ++ raw
    show (CE e)                    = show e
-- }}}

data CliOptions = CliOptions {
    mList         :: Bool,
    mMasterBinary :: Maybe String} 
  deriving (Data, Typeable, Show, Eq)

-- | Set of settings for imm
data Settings = Settings {
    mStateDirectory :: PortableFilePath,
    mError          :: Maybe String}

type FeedGroup = (FeedSettings, [String]) 

data FeedSettings = FeedSettings {
    mMailDirectory  :: PortableFilePath}

type ImmFeed = (URI, Feed)

data Mail = Mail {
    mReturnPath         :: String,
    mDate               :: Maybe ZonedTime,
    mFrom               :: String,
    mSubject            :: T.Text,
    mMIME               :: String,
    mCharset            :: String,
    mContentDisposition :: String,
    mContent            :: T.Text}

-- | Set of reference directories, typically used to build FilePath-s
data RefDirs = RefDirs {
    mHome          :: FilePath,        -- ^ Home directory
    mTemporary     :: FilePath,        -- ^ Temporary files directory
    mConfiguration :: FilePath,        -- ^ Configuration directory
    mData          :: FilePath}        -- ^ Data directory

type PortableFilePath = RefDirs -> FilePath

