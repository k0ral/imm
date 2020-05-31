module Prelude (io, module Relude, module Relude.Extra.Map) where

import           Relude           hiding (Handle, appendFile, force, readFile, writeFile)
import           Relude.Extra.Map (lookup)

io :: MonadIO m => IO a -> m a
io = liftIO
