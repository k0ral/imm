module Main where

-- {{{ Imports
import Imm.Boot
import Imm.Config
import Imm.Types

import System.FilePath
-- }}}

main :: IO ()
main = imm myFeedGroups mySettings

mySettings :: Settings
mySettings = defaultSettings

myFeedGroups :: [FeedGroup]
myFeedGroups = [
    (exampleFeeds, ["http://planet.haskell.org/rss20.xml"])]

exampleFeeds :: FeedSettings
exampleFeeds = FeedSettings {
    mMailDirectory = \refDirs -> (mHome refDirs) </> "feeds"}
