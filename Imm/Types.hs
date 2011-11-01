{-# LANGUAGE DeriveDataTypeable #-}
module Imm.Types where

import Network.URI

import System.Console.CmdArgs

import Text.Feed.Types



data CliOptions = CliOptions {
    mParameter :: Maybe String
} deriving (Data, Typeable, Show, Eq)

-- | 
data Parameters = Parameters {
    mCacheDirectory :: Maybe String,
    mMailTo         :: Maybe String,
    mSMTP           :: Maybe String,
    mFeedURIs       :: [String],             -- ^ Feeds list
    mError          :: Maybe String          -- ^ Error                                                                                                                           
}

data ImmFeed = ImmFeed {
    mURI  :: URI,
    mFeed :: Feed
}