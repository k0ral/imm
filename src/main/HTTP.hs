{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- | Implementation of "Imm.HTTP" based on "Network.HTTP.Client".
module HTTP (mkHandle) where

-- {{{ Imports
import           Imm.HTTP
import           Imm.Logger           hiding (Handle)
import qualified Imm.Logger           as Logger
import           Imm.Pretty

import           Pipes.ByteString
import           System.Process.Typed
-- }}}

mkHandle :: Logger.Handle IO -> Handle IO
mkHandle logger = Handle $ \uri f ->
  withProcessWait (httpie uri) $ \httpieProcess -> do
    log logger Debug $ pretty $ show @String httpieProcess
    f (fromHandle $ getStdout httpieProcess)

  where httpie uri = proc "http" ["--timeout", "10", "--follow", "--print", "b", "GET", show $ prettyURI uri]
          & setStdin nullStream
          & setStdout createPipe
          & setStderr nullStream
