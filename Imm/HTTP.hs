{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | HTTP client related functions
module Imm.HTTP where

-- {{{ Imports
import Imm.Types
import Imm.Util as U

import Control.Exception as E
import Control.Monad.Error hiding(forM_, mapM_)

import Data.ByteString.Lazy as B
import Data.Functor

import Network.HTTP.Conduit as H
import Network.URI

import Prelude hiding(catch)
-- }}}


getRaw :: (MonadIO m, MonadError ImmError m) => URI -> m ByteString
getRaw uri = do
    logVerbose $ "Downloading " ++ show uri
    simpleHTTP $ show uri

-- | Monad-agnostic version of 'simpleHttp'
--simpleHTTP :: (MonadIO m, MonadError ImmError m) => String => m ByteString
simpleHTTP uri = do
    result <- io $ (Right <$> simpleHttp uri) `catch` (return . Left . IOE) `catch` (return . Left . HTTPError) `catch` (return . Left . TLSError)
    either throwError return result
    
