{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Implementation of "Imm.HTTP" based on "Pipes.HTTP".
module HTTP (mkHandle) where

-- {{{ Imports
import Imm.HTTP
import Imm.Pretty
import Network.HTTP.Types.Header
import Pipes.HTTP

-- }}}

headers ∷ [Header]
headers = [(hUserAgent, "imm/1.0")]

mkHandle ∷ m ~ IO ⇒ m (Handle m)
mkHandle = do
  manager ← newManager tlsManagerSettings

  return $
    Handle $ \uri f → do
      request ← parseUrlThrow $ show $ prettyURI uri
      withHTTP request{requestHeaders = headers} manager f
