-- | Default settings
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
    mBodyBuilder    = defaultBodyBuilder
}

-- | Default builder for mail body.
-- Writes the item's URI and then the item's content.
defaultBodyBuilder :: (Item, Feed) -> T.Text
defaultBodyBuilder (item, _feed) = 
    T.unlines $ map (flip ($) item) [T.pack . getItemLinkNM, getItemContent]
