-- | XML module abstracts over the parsing of RSS/Atom feeds.
--
-- This module follows the [Handle pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html).
--
-- > import qualified Imm.XML as XML
module Imm.XML where

-- {{{ Imports
import           Imm.Feed

import           URI.ByteString
-- }}}

newtype Handle m = Handle
  { parseXml :: URI -> LByteString -> m (FeedDefinition, [FeedItem])
  }
