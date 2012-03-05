module Main where

-- {{{ Imports
import Imm.Config
import Imm.Core
import Imm.Types

import System.FilePath
-- }}}

main :: IO ()
main = imm myFeedGroups myGlobalSettings

myGlobalSettings :: Parameters
myGlobalSettings = defaultGlobalSettings

myFeedGroups :: [FeedGroup]
myFeedGroups = [
    (exampleFeeds, ["http://planet.haskell.org/rss20.xml"])]

exampleFeeds :: FeedSettings
exampleFeeds = FeedSettings {
    mMailDirectory = \refDirs -> (mHome refDirs) </> "feeds"}
