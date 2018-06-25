{-# LANGUAGE NoImplicitPrelude #-}
-- | XML module abstracts over the parsing of RSS/Atom feeds.
module Imm.XML where

-- {{{ Imports
import           Imm.Feed
import           Imm.Prelude

import           URI.ByteString
-- }}}

-- | Monad capable of parsing XML into a 'Feed' (RSS or Atom).
class MonadThrow m => MonadXmlParser m where
  parseXml :: URI -> LByteString -> m Feed
