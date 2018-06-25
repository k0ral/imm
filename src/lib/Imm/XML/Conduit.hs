{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
-- | Implementation of "Imm.XML" based on 'Conduit'.
module Imm.XML.Conduit where

-- {{{ Imports
import           Imm.Feed
import           Imm.Prelude
import           Imm.XML

import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Reader
import           Data.Conduit
import           Data.XML.Types
import           Text.Atom.Conduit.Parse
import           Text.RSS.Conduit.Parse
import           Text.RSS1.Conduit.Parse
import           Text.XML.Stream.Parse
import           URI.ByteString
-- }}}

-- | A pre-process 'Conduit' can be set to alter the raw XML before feeding it to the parser,
-- depending on the feed 'URI'
newtype XmlParser = XmlParser (forall m . Monad m => URI -> ConduitT Event Event m ())

-- | 'Conduit' based implementation
instance (MonadIO m, MonadCatch m) => MonadXmlParser (ReaderT XmlParser m) where
  parseXml uri bytestring = do
    XmlParser preProcess <- ask
    lift $ runConduit $ parseLBS def bytestring .| preProcess uri .| force "Invalid feed" ((fmap Atom <$> atomFeed) `orE` (fmap Rss <$> rssDocument) `orE` (fmap Rss <$> rss1Document))

-- | Forward all 'Event's without any pre-process
defaultXmlParser :: XmlParser
defaultXmlParser = XmlParser $ const $ fix $ \loop -> await >>= maybe (return ()) (yield >=> const loop)
