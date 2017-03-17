{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE EmptyDataDecls         #-}
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
import           Control.Applicative             as X
import           Control.Comonad
import           Control.Comonad.Cofree
import           Control.Exception.Safe          as X
import           Control.Monad                   as X (MonadPlus (..), unless,
                                                       void, when)
import           Control.Monad.IO.Class          as X
import           Control.Monad.Trans.Free        (FreeF (..), FreeT (..))

import           Data.Bifunctor                  as X
import qualified Data.ByteString                 as B (ByteString ())
import qualified Data.ByteString.Lazy            as LB (ByteString ())
import           Data.Containers                 as X
import           Data.Either                     as X
import           Data.Foldable                   as X (forM_)
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.IOData                     as X
import           Data.Map                        as X (Map)
import           Data.Maybe                      as X hiding (catMaybes)
import           Data.Monoid                     as X hiding (Product, Sum)
import           Data.Monoid.Textual             as X (TextualMonoid (),
                                                       fromText)
import           Data.MonoTraversable.Unprefixed as X hiding (forM_, mapM_)
import           Data.Ord                        as X
import           Data.Sequences                  as X
import           Data.String                     as X (IsString (..))
import           Data.Tagged
import qualified Data.Text                       as T (Text ())
import qualified Data.Text.Lazy                  as LT (Text ())
import           Data.Traversable                as X (for, forM)
import           Data.Typeable                   as X

import qualified GHC.Show                        as Show

import           Prelude                         as X hiding (all, and, any,
                                                       break, concat, concatMap,
                                                       drop, dropWhile, elem,
                                                       filter, foldMap, foldr,
                                                       getLine, length, lines,
                                                       log, lookup, notElem,
                                                       null, or, product,
                                                       readFile, replicate,
                                                       reverse, sequence_, show,
                                                       span, splitAt, sum, take,
                                                       takeWhile, unlines,
                                                       unwords, words,
                                                       writeFile)

import           System.IO                       as X (stderr, stdout)

import           Text.PrettyPrint.ANSI.Leijen    as X (Doc, Pretty (..), angles,
                                                       brackets, equals, hsep,
                                                       indent, space, text,
                                                       vsep, (<+>))

import           Text.PrettyPrint.ANSI.Leijen    (line)
-- }}}

-- * Free monad utilities

-- | Right-associative tuple type-constructor
type a ::: b = (a, b)
infixr 0 :::

-- | Right-associative tuple data-constructor
(+:) :: a -> b -> (a,b)
(+:) a b = (a, b)
infixr 0 +:

(*:) :: (Functor f, Functor g) => (a -> f a) -> (b -> g b) -> (a, b) -> Product f g (a, b)
(*:) f g (a,b) = Pair ((,b) <$> f a) ((a,) <$> g b)
infixr 0 *:


data HLeft
data HRight
data HId
data HNo

type family Contains a b where
  Contains a a         = HId
  Contains a (Sum a b) = HLeft
  Contains a (Sum b c) = (HRight, Contains a c)
  Contains a b         = HNo

class Sub i sub sup where
  inj' :: Tagged i (sub a -> sup a)

instance Sub HId a a where
  inj' = Tagged id

instance Sub HLeft a (Sum a b) where
  inj' = Tagged InL

instance (Sub x f g) => Sub (HRight, x) f (Sum h g) where
  inj' = Tagged $ InR . proxy inj' (Proxy :: Proxy x)


-- | A constraint @f :<: g@ expresses that @f@ is subsumed by @g@,
-- i.e. @f@ can be used to construct elements in @g@.
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance (Functor f, Functor g, Sub (Contains f g) f g) => f :<: g where
  inj = proxy inj' (Proxy :: Proxy (Contains f g))


class (Monad m, Functor f, Functor g) => PairingM f g m | f -> g where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

instance (Monad m) => PairingM Identity Identity m where
  pairM f (Identity a) (Identity b) = f a b

instance (PairingM f f' m, PairingM g g' m) => PairingM (Sum f g) (Product f' g') m where
  pairM p (InL x) (Pair a _) = pairM p x a
  pairM p (InR x) (Pair _ b) = pairM p x b

instance (PairingM f f' m, PairingM g g' m) => PairingM (Product f g) (Sum f' g') m where
  pairM p (Pair a _) (InL x) = pairM p a x
  pairM p (Pair _ b) (InR x) = pairM p b x

interpret :: (PairingM f g m) => (a -> b -> m r) -> Cofree f a -> FreeT g m b -> m r
interpret p eval program = do
  let a = extract eval
  b <- runFreeT program
  case b of
    Pure x  -> p a x
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
