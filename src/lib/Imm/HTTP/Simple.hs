{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of "Imm.HTTP" based on "Network.HTTP.Client".
module Imm.HTTP.Simple (mkHandle, defaultManager, module Reexport) where

-- {{{ Imports
import           Imm.HTTP
import           Imm.Prelude
import           Imm.Pretty

import           Data.CaseInsensitive
import           Network.Connection         as Reexport
import           Network.HTTP.Client        as Reexport
import           Network.HTTP.Client.TLS    as Reexport
import           URI.ByteString
-- }}}

mkHandle :: MonadBase IO m => Manager -> Handle m
mkHandle manager = Handle
  { httpGet = liftBase . httpGet' manager
  }

-- | Default manager uses TLS and no proxy
defaultManager :: IO Manager
defaultManager = newManager $ mkManagerSettings (TLSSettingsSimple False False False) Nothing


-- | Perform an HTTP GET request and return the response body
httpGet' :: Manager -> URI -> IO LByteString
httpGet' manager uri = do
  request <- makeRequest uri
  responseBody <$> httpLbs request manager
    -- codec'   <- reader $ view (config.codec)
    -- return $ response $=+ decode codec'

parseRequest' :: MonadThrow m => URI -> m Request
parseRequest' = parseRequest . show . prettyURI

-- | Build an HTTP request for given URI
makeRequest :: URI -> IO Request
makeRequest uri = do
  req <- parseRequest' uri
  return $ req { requestHeaders = [
    (mk "User-Agent", "Mozilla/4.0"),
    (mk "Accept", "*/*")]}
