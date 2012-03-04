module Imm.Config where

import Imm.Types

-- | Default configuration.
defaultGlobalSettings :: Parameters
defaultGlobalSettings = Parameters {
    mCacheDirectory  = Nothing,
    mError           = Nothing
}
