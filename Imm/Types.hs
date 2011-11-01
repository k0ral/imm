module Imm.Types where

import Text.Feed.Types

-- | 
data Parameters = Parameters {
    mFeedURIs :: [String],             -- ^ Feeds list
    mError :: Maybe String          -- ^ Error                                                                                                                           
}

data ImmFeed = ImmFeed {
    mURI  :: String,
    mFeed :: Feed
}