module Imm.Config where

import Imm.Types

import System.FilePath

-- | Default configuration.
defaultSettings :: Settings
defaultSettings = Settings {
    mStateDirectory = (</> "state") . mConfiguration,
    mFeedGroups     = []
}
