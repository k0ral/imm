{-# LANGUAGE FlexibleContexts #-}
module Imm.Prelude (module Imm.Prelude, module X) where

-- {{{ Imports
import           Control.Applicative             as X
import           Control.Exception.Safe          as X
import           Control.Monad                   as X (MonadPlus (..), unless,
                                                       void, when)
import           Control.Monad.Base              as X
import           Control.Monad.IO.Class          as X
import           Control.Monad.Trans             as X (lift)
import           Data.Bifunctor                  as X
import qualified Data.ByteString                 as B (ByteString)
import qualified Data.ByteString.Lazy            as LB (ByteString)
import           Data.Containers                 as X
import           Data.Either                     as X
import           Data.Foldable                   as X (forM_)
import           Data.Maybe                      as X hiding (catMaybes)
import           Data.Monoid.Textual             as X (TextualMonoid, fromText)
import           Data.MonoTraversable.Unprefixed as X hiding (forM_, mapM_)
import           Data.Ord                        as X
import           Data.Sequences                  as X
import           Data.String                     as X (IsString (..))
import qualified Data.Text                       as T (Text)
import           Data.Text.IO                    as X (getLine, putStr,
                                                       putStrLn)
import qualified Data.Text.Lazy                  as LT (Text)
import           Data.Traversable                as X (for, forM)
import qualified GHC.Show                        as Show
import           Prelude                         as X hiding (all, and, any,
                                                       break, concat, concatMap,
                                                       drop, dropWhile, elem,
                                                       filter, foldMap, foldr,
                                                       getLine, length, lines,
                                                       log, lookup, notElem,
                                                       null, or, product,
                                                       putStr, putStrLn,
                                                       readFile, replicate,
                                                       reverse, sequence_, show,
                                                       span, splitAt, sum, take,
                                                       takeWhile, unlines,
                                                       unwords, words,
                                                       writeFile)
import           System.IO                       as X (IOMode (..), stderr,
                                                       stdout)
-- }}}

-- * Shortcuts

type LByteString = LB.ByteString
type ByteString = B.ByteString
type LText = LT.Text
type Text = T.Text

-- | Shortcut to 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- * Generalisation

-- | Generic 'Show.show'
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show
