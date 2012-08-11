{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, KindSignatures #-}
module Imm.HTTP where

-- {{{ Imports
import Imm.Types
import Imm.Util as U

import Control.Exception as E
import Control.Monad.Error hiding(forM_, mapM_)

import Data.ByteString.Lazy as BL
import Data.ByteString.Char8 as BC
import Data.CaseInsensitive
import Data.Functor

import Network.HTTP.Conduit as H
import Network.URI

import Prelude hiding(catch)
-- }}}

-- | Perform an HTTP GET request and return the response body as raw 'ByteString'
getRaw :: (MonadIO m, MonadError ImmError m) => URI -> m BL.ByteString
getRaw uri = do
    logVerbose $ "Downloading " ++ show uri
    req <- request $ show uri
    res <- withManager' (httpLbs req)
    return $ responseBody res

-- | Monad-agnostic version of 'withManager'
withManager' f = do
    res <- timeout 11000000 $ (Right <$> withManager f) `catch` (return . Left . IOE) `catch` (return . Left . HTTPError) `catch` (return . Left . TLSError)
    either throwError return res

-- | Monad-agnostic version of 'parseUrl'
parseURL :: forall (m :: * -> *) (m' :: * -> *) . (MonadIO m, MonadError ImmError m) => String -> m (Request m')
parseURL uri = do
    result <- io $ (Right <$> parseUrl uri) `catch` (return . Left . HTTPError)
    either throwError return result
    
-- | Build an HTTP request for given URI
request :: (MonadError ImmError m, MonadIO m) => String -> m (Request a)
request uri = do
    req <- parseURL uri
    return $ req { requestHeaders = [
        (mk $ BC.pack "User-Agent", BC.pack "Mozilla/4.0"),
        (mk $ BC.pack "Accept", BC.pack "*/*")]}
