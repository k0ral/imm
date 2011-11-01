module Main where

-- {{{ Imports
import Imm.Core
import Imm.Types
-- }}}


main :: IO ()
main = imm myParameters


myParameters :: Parameters
myParameters = defaultParameters {
  --mMailTo = Just "mail@address",
  --mSMTP   = Just "your.smtp",
  --mFeedURIs = ["feed_url"]
}

