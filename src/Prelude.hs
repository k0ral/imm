module Prelude (for, io, headFail, headThrow, module Relude, module Relude.Extra.Map) where

import           Relude           hiding (Handle, appendFile, force, readFile, stdout, writeFile)
import           Relude.Extra.Map (lookup)
import           Control.Exception.Safe

io :: MonadIO m => IO a -> m a
io = liftIO

for :: [a] -> (a -> b) -> [b]
for = flip map

headThrow :: MonadThrow m => Exception e => e -> [a] -> m a
headThrow _ (a:_) = return a
headThrow e _ = throwM e

headFail :: MonadFail m => String -> [a] -> m a
headFail _ (a:_) = return a
headFail e _ = fail e
