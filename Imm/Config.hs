module Imm.Config where

import Imm.Types

-- | Default configuration.
defaultParameters :: Parameters
defaultParameters = Parameters {
    mCacheDirectory = Nothing,
    mFeedURIs       = [],
    mMailDirectory  = defaultMailDirectory,
    mError          = Nothing
}

defaultMailDirectory :: PortableFilePath
defaultMailDirectory = const "feeds"
