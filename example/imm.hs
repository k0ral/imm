module Main where

-- {{{ Imports
import Imm.Config
import Imm.Core
import Imm.Types

import System.FilePath
-- }}}

main :: IO ()
main = imm myParameters

myParameters :: Parameters
myParameters = defaultParameters {
    mFeedURIs      = myFeeds,
    mMailDirectory = myMailDirectory
}

myFeeds :: [String]
myFeeds = [
    "http://planet.haskell.org/rss20.xml"]

myMailDirectory :: PortableFilePath
myMailDirectory refDirs = (mHome refDirs) </> "feeds"
