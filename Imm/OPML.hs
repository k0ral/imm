module Imm.OPML where

-- {{{ Imports
import Text.OPML.Reader
import Text.OPML.Syntax
import Text.XML.Light.Types
-- }}}

-- | Parse an OPML string and return a list of tuples (category title, feed URIs).
read :: String -> Maybe [(String, [String])]
read rawOPML = do
    opml <- parseOPMLString rawOPML
    let groups     = opmlBody opml
        groupNames = map opmlText groups
        feeds      = \group -> opmlOutlineChildren group
        feedURI    = \feed -> (concat . map attrVal) . (filter ((== "xmlUrl") . qName . attrKey)) . opmlOutlineAttrs $ feed
    
    return $ zip groupNames (map (map feedURI) (map feeds groups))
