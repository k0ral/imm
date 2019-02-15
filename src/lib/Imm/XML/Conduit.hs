{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
-- | Implementation of "Imm.XML" based on 'Conduit'.
module Imm.XML.Conduit (module Imm.XML.Conduit, module Imm.XML) where

-- {{{ Imports
import           Imm.Feed
import           Imm.XML

import           Control.Exception.Safe
import           Data.Conduit
import           Data.XML.Types
import           Text.Atom.Conduit.Parse
import           Text.RSS.Conduit.Parse
import           Text.RSS1.Conduit.Parse
import           Text.XML.Stream.Parse   as XML
import           URI.ByteString
-- }}}

-- | A pre-process 'Conduit' can be set to alter the raw XML before feeding it to the parser,
-- depending on the feed 'URI'
newtype XmlParser = XmlParser (forall m . Monad m => URI -> ConduitT Event Event m ())

-- | 'Conduit' based implementation
mkHandle :: MonadIO m => MonadCatch m => XmlParser -> Handle m
mkHandle (XmlParser preProcess) = Handle
  { parseXml = \uri bytestring -> liftIO $ runConduit $ parseLBS def bytestring .| preProcess uri .| XML.force "Invalid feed" ((fmap Atom <$> atomFeed) `orE` (fmap Rss <$> rssDocument) `orE` (fmap Rss <$> rss1Document))
  }

-- | Forward all 'Event's without any pre-process
defaultXmlParser :: XmlParser
defaultXmlParser = XmlParser $ const $ fix $ \loop -> await >>= maybe (return ()) (yield >=> const loop)
