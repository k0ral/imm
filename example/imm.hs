module Main where

import Imm

-- The 'main' function must call 'imm' with a feed list
main :: IO ()
main = imm myFeeds

-- Feeds are a list of tuples (custom settings, uri)
myFeeds :: [ConfigFeed]
myFeeds = zip (repeat id) myUris

-- Uris are bare String and will be parsed inside imm
myUris =  ["http://planet.haskell.org/rss20.xml", "http://xkcd.com/rss.xml"]
