{-# LANGUAGE NoImplicitPrelude #-}
-- | Simple interpreter to parse XML into 'Feed', based on 'Conduit'.
module Imm.XML.Simple where

-- {{{ Imports
import           Imm.Feed
import           Imm.Prelude
import           Imm.XML

import           Control.Monad
import           Control.Monad.Fix

import           Data.Conduit
import           Data.XML.Types

import           Text.Atom.Conduit.Parse
import           Text.RSS.Conduit.Parse
import           Text.RSS1.Conduit.Parse
import           Text.XML.Stream.Parse

import           URI.ByteString
-- }}}

-- | A 'Conduit' to alter the raw XML before feeding it to the parser, depending on the feed 'URI'
type PreProcess m = URI -> ConduitT Event Event m ()

-- | Interpreter for 'XmlParserF'
mkCoXmlParser :: (MonadIO m, MonadCatch m) => PreProcess m -> CoXmlParserF m (PreProcess m)
mkCoXmlParser preProcess = CoXmlParserF coParse where
  coParse uri bytestring = handleAny (\e -> return (Left e, preProcess)) $ do
    result <- runConduit $ parseLBS def bytestring .| preProcess uri .| force "Invalid feed" ((fmap Atom <$> atomFeed) `orE` (fmap Rss <$> rssDocument) `orE` (fmap Rss <$> rss1Document))
    return (Right result, preProcess)

-- | Default pre-process always forwards all 'Event's
defaultPreProcess :: Monad m => PreProcess m
defaultPreProcess _ = fix $ \loop -> await >>= maybe (return ()) (yield >=> const loop)
