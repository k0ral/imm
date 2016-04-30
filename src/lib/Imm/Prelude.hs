{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Imm.Prelude (module Imm.Prelude, module X) where

-- {{{ Imports
import           Control.Applicative          as X
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Monad                as X hiding (filterM, replicateM)
import           Control.Monad.Catch          as X
import           Control.Monad.IO.Class       as X
import           Control.Monad.Trans.Free     (FreeF (..), FreeT (..))

import           Data.Bifunctor               as X
import qualified Data.ByteString              as B (ByteString ())
import qualified Data.ByteString.Lazy         as LB (ByteString ())
import           Data.Comp.Ops                as X
import           Data.Containers              as X
import           Data.Functor.Identity
import           Data.IOData                  as X
import           Data.Map                     as X (Map)
import           Data.Maybe                   as X hiding (catMaybes)
import           Data.Monoid                  as X
import           Data.Monoid.Textual          as X (TextualMonoid (), fromText)
import           Data.MonoTraversable         as X
import           Data.Ord                     as X
-- import           Data.Semigroup               as X hiding (option)
import           Data.Sequences               as X
import           Data.Sequences.Lazy          as X
import           Data.String                  as X (IsString (..))
import qualified Data.Text                    as T (Text ())
import qualified Data.Text.Lazy               as LT (Text ())
import           Data.Textual.Encoding        as X
import           Data.Typeable                as X

import qualified GHC.Show                     as Show

import           Prelude                      as X hiding (break, drop,
                                                    dropWhile, elem, filter,
                                                    getLine, lines, log, lookup,
                                                    notElem, readFile,
                                                    replicate, reverse, show,
                                                    span, splitAt, take,
                                                    takeWhile, unlines, unwords,
                                                    words, writeFile)

import           System.IO                    as X (stderr, stdout)

import           Text.PrettyPrint.ANSI.Leijen as X (Doc, Pretty (..), angles,
                                                    brackets, equals, hsep,
                                                    indent, space, text, vsep,
                                                    (<+>))

import           Text.PrettyPrint.ANSI.Leijen (line)
-- }}}

-- * Free monad utilities

-- | Right-associative tuple type-constructor
type a ::: b = (a, b)
infixr 0 :::

-- | Right-associative tuple data-constructor
(>:) :: a -> b -> (a,b)
(>:) a b = (a, b)
infixr 0 >:

(*:*) :: (Functor f, Functor g) => (a -> f a) -> (b -> g b) -> (a, b) -> (f :*: g) (a, b)
(*:*) f g (a,b) = ((,b) <$> f a) :*: ((a,) <$> g b)
infixr 0 *:*


class (Monad m, Functor f, Functor g) => PairingM f g m | f -> g where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

instance (Monad m) => PairingM Identity Identity m where
  pairM f (Identity a) (Identity b) = f a b

instance (PairingM f f' m, PairingM g g' m) => PairingM (f :+: g) (f' :*: g') m where
  pairM p (Inl x) (a :*: _) = pairM p x a
  pairM p (Inr x) (_ :*: b) = pairM p x b

instance (PairingM f f' m, PairingM g g' m) => PairingM (f :*: g) (f' :+: g') m where
  pairM p (a :*: _) (Inl x) = pairM p a x
  pairM p (_ :*: b) (Inr x) = pairM p b x

interpret :: (PairingM f g m) => (a -> b -> m r) -> Cofree f a -> FreeT g m b -> m r
interpret p eval program = do
  let a = extract eval
  b <- runFreeT program
  case b of
    Pure x -> p a x
    Free gs -> pairM (interpret p) (unwrap eval) gs

-- * Shortcuts

type LByteString = LB.ByteString
type ByteString = B.ByteString
type LText = LT.Text
type Text = T.Text

-- | Generic 'Show.show'
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show

-- | Shortcut to 'liftIO'
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Infix operator for 'line'
(<++>) :: Doc -> Doc -> Doc
x <++> y = x <> line <> y
