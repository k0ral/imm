{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Implementation of "Imm.XML" based on 'Conduit'.
module XML (module XML, module Imm.XML) where

-- {{{ Imports

import Control.Exception.Safe
import Data.Conduit
import Data.XML.Types
import Imm.Feed
import Imm.XML
import Text.XML.Stream.Parse as XML
import URI.ByteString

-- }}}

-- | A pre-process 'Conduit' can be set to alter the raw XML before feeding it to the parser,
-- depending on the feed 'URI'
newtype XmlParser = XmlParser (∀ m. Monad m ⇒ URI → ConduitT Event Event m ())

-- | 'Conduit' based implementation
mkHandle ∷ MonadIO m ⇒ MonadCatch m ⇒ XmlParser → Handle m
mkHandle (XmlParser preProcess) =
  Handle
    { parseXml = \uri bytestring →
        liftIO $
          runConduit $
            parseLBS def bytestring
              .| preProcess uri
              .| XML.force "Invalid feed" feedC
    }

-- | Forward all 'Event's without any pre-process
defaultXmlParser ∷ XmlParser
defaultXmlParser = XmlParser $ const $ fix $ \loop → await >>= maybe (return ()) (yield >=> const loop)
