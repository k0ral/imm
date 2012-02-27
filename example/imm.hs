module Main where

-- {{{ Imports
import Imm.Core
import Imm.Types
-- }}}

main :: IO ()
main = imm myParameters

myParameters :: Parameters
myParameters = defaultParameters {
  mFeedURIs = myFeeds
}

myFeeds :: [String]
myFeeds = [
    "http://www.archlinux.org/feeds/news/"]
