module Imm.Util where

-- {{{ Imports
import Codec.Binary.UTF8.String

import Data.Time.Clock
import Data.Time.Format

--import Network.URI

import System.Locale
-- }}}

escapeFileName :: Char -> String
escapeFileName '/' = "|"
escapeFileName x   = x:[]
    
stringToUTC :: String -> Maybe UTCTime
stringToUTC = parseTime defaultTimeLocale "%a, %e %b %Y %T %z"

decodeIfNeeded :: String -> String
decodeIfNeeded text = case isUTF8Encoded text of
    False -> text
    _     -> decodeString text
