module Main where

-- {{{ Imports
import Imm

import System.Directory
import System.Environment.XDG.BaseDir
-- }}}

-- The 'main' function must call 'imm' with a feed list
main :: IO ()
main = imm myFeeds

-- Feeds are a list of tuples (custom settings, uri)
myFeeds :: FeedList
myFeeds = zip (repeat mySettings) myUris

-- Custom settings transform default settings (cf 'Imm.Types.Settings')
mySettings :: CustomSettings
mySettings = id

-- Uris are bare String and will be parsed inside imm
myUris =  ["http://planet.haskell.org/rss20.xml", "http://xkcd.com/rss.xml"]
