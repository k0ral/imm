{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Simple HTTP client interpreter.
-- For more information, please consult "Network.HTTP.Client".
module Imm.HTTP.Simple (defaultManager, mkCoHttpClient, module Reexport) where

-- {{{ Imports
import           Imm.HTTP
import           Imm.Prelude
import           Imm.Pretty

import           Data.CaseInsensitive

import           Network.Connection      as Reexport
import           Network.HTTP.Client     as Reexport
import           Network.HTTP.Client.TLS as Reexport

import           URI.ByteString
-- }}}

-- | Interpreter for 'HttpClientF'
mkCoHttpClient :: (MonadIO m, MonadCatch m) => Manager -> CoHttpClientF m Manager
mkCoHttpClient manager = CoHttpClientF coGet where
  coGet uri = handleAny (\e -> return (Left e, manager)) $ do
    result <- httpGet manager uri
    return (Right result, manager)

-- | Default manager uses TLS and no proxy
defaultManager :: IO Manager
defaultManager = newManager $ mkManagerSettings (TLSSettingsSimple False False False) Nothing


-- | Perform an HTTP GET request and return the response body
httpGet :: (MonadIO m, MonadThrow m)
    => Manager -> URI -> m LByteString
httpGet manager uri = do
  request <- makeRequest uri
  responseBody <$> io (httpLbs request manager)
    -- codec'   <- reader $ view (config.codec)
    -- return $ response $=+ decode codec'

parseRequest' :: (MonadThrow m) => URI -> m Request
parseRequest' = parseRequest . show . prettyURI

-- | Build an HTTP request for given URI
makeRequest :: (MonadIO m, MonadThrow m) => URI -> m Request
makeRequest uri = do
  req <- parseRequest' uri
  return $ req { requestHeaders = [
    (mk "User-Agent", "Mozilla/4.0"),
    (mk "Accept", "*/*")]}
