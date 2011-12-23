{-# LANGUAGE DeriveDataTypeable #-}
module Imm.Types where

-- {{{ Imports
import Network.URI

import Data.Time.Clock

import System.Console.CmdArgs

import Text.Feed.Types
-- }}}


data CliOptions = CliOptions {
    mParameter :: Maybe String
} deriving (Data, Typeable, Show, Eq)

-- | 
data Parameters = Parameters {
    mCacheDirectory :: Maybe String,
    mFeedURIs       :: [String],             -- ^ Feeds list
    mMailBox        :: FilePath,
    mError          :: Maybe String          -- ^ Error                                                                                                                           
}

data ImmFeed = ImmFeed {
    mURI  :: URI,
    mFeed :: Feed
}

data Mail = Mail {
    mReturnPath  :: String,
    mDate        :: Maybe UTCTime,
    mFrom        :: String,
    mSubject     :: String,
    mMIME        :: String,
    mCharset     :: String,
    mContentDisposition :: String,
    mContent     :: String
}