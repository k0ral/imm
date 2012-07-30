module Imm.Config where

-- {{{ Imports
import Imm.Feed
import Imm.Types

import qualified Data.Text.Lazy as T

import System.FilePath

import Text.Feed.Query
import Text.Feed.Types
-- }}}

-- | Default configuration.
defaultSettings :: Settings
defaultSettings = Settings {
    mStateDirectory = (</> "state") . mConfiguration,
    mMaildir        = (</> "feeds") . mHome,
    mFromBuilder    = \(item, feed) -> maybe (getFeedTitle feed) id $ getItemAuthor item,
    mSubjectBuilder = \(item, _feed) -> T.pack . maybe "Untitled" id $ getItemTitle item,
    mBodyBuilder    = \(item, _feed) -> defaultBodyBuilder item
}

defaultBodyBuilder :: Item -> T.Text
defaultBodyBuilder item = 
    T.unlines $ map (flip ($) item) [T.pack . getItemLinkNM, getItemContent]
