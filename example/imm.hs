module Main where

-- {{{ Imports
import Imm.Boot
import Imm.Config
import Imm.Types
import Imm.Util 

import System.Directory
import System.Environment.XDG.BaseDir
-- }}}

main :: IO ()
main = imm myConf


myConf :: FeedList
myConf = zip (repeat mySettings) feeds


 
feeds =  ["http://planet.haskell.org/rss20.xml","http://xkcd.com/rss.xml" ]
