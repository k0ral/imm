-- | Default settings
module Imm.Config where

-- {{{ Imports
import Imm.Feed
import Imm.Types
import Imm.Util

import Control.Monad.Trans

import Data.Char
import Data.Default
import Data.Foldable hiding(concat)
import qualified Data.Text.Lazy as T

import System.FilePath

import Text.Feed.Query
-- }}}


instance Default Settings where
    def = Settings {
        mStateDirectory = (</> "state") . mConfiguration,
        mMaildir        = (</> "feeds") . mHome,
        mFromBuilder    = \(item, feed) -> maybe (getFeedTitle feed) id $ getItemAuthor item,
        mSubjectBuilder = \(item, _feed) -> T.pack . maybe "Untitled" id $ getItemTitle item,
        mBodyBuilder    = \(item, _feed) -> T.unlines $ map (flip ($) item) [T.pack . getItemLinkNM, getItemContent]}


addFeeds :: MonadIO m => [(String, [String])] -> m ()
addFeeds feeds = io . forM_ feeds $ \(groupTitle, uris) -> do
    putStrLn $ "-- Group " ++ groupTitle
    putStrLn $ map toLower (concat . words $ groupTitle) ++ " = ["
    forM_ uris (\uri -> putStrLn $ "    " ++ show uri ++ ",")
    putStrLn "]"
    putStrLn ""
    
