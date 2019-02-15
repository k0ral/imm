{-# LANGUAGE ScopedTypeVariables #-}
module Imm.Error (module Imm.Error) where

-- {{{ Imports
import           Control.Exception.Safe
-- }}}

liftE :: (MonadThrow m, Exception e) => Either e a -> m a
liftE (Left e)  = throwM e
liftE (Right a) = return a

-- | Wrap a 'Maybe' value in 'MonadError'
failWith, (<!>) :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
failWith x e = maybe (throwM e) return x
(<!>) = failWith
