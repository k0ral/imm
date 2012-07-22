module Main where

-- {{{ Imports
import Imm.Boot
import Imm.Config
import Imm.Types

import System.FilePath
-- }}}

main :: IO ()
main = imm mySettings

mySettings :: Settings
mySettings = defaultSettings {
    mFeedGroups = myFeedGroups }

myFeedGroups :: [FeedGroup]
myFeedGroups = [
    (exampleFeeds, ["http://planet.haskell.org/rss20.xml"])]

exampleFeeds :: FeedSettings
exampleFeeds = FeedSettings {
    mMaildir = \refDirs -> (mHome refDirs) </> "feeds"}
