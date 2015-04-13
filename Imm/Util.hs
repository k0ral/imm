module Imm.Util (
    module X,
    (>/>),
    io,
) where

-- {{{ Imports
import Control.Applicative as X
import Control.Conditional as X hiding(unless)
import Control.Monad.Base as X

import Data.Char as X
import Data.Default as X
import Data.Either as X
import Data.Functor as X
import Data.List as X hiding(foldl, init, sum)
import Data.Maybe as X

import System.FilePath
-- }}}


-- | Like '</>' with first argument in IO to build platform-dependent paths.
(>/>) :: (MonadBase IO m) => IO FilePath -> FilePath -> m FilePath
(>/>) a b = io $ (</> b) <$> a

-- | Shortcut to 'liftBase' with 'IO' as base monad
io :: MonadBase IO m => IO a -> m a
io = liftBase
