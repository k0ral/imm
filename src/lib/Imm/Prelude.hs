{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
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
import qualified Data.ByteString                 as B (ByteString)
import qualified Data.ByteString.Lazy            as LB (ByteString)
import           Data.Containers                 as X
import           Data.Either                     as X
import           Data.Extensible.Class           as X
import           Data.Foldable                   as X (forM_)
import           Data.Functor.Identity
import           Data.IOData                     as X
import           Data.Map                        as X (Map)
import           Data.Maybe                      as X hiding (catMaybes)
import           Data.Monoid                     as X hiding (Product, Sum)
import           Data.Monoid.Textual             as X (TextualMonoid, fromText)
import           Data.MonoTraversable.Unprefixed as X hiding (forM_, mapM_)
import           Data.Ord                        as X
import           Data.Sequences                  as X
import           Data.String                     as X (IsString (..))
import qualified Data.Text                       as T (Text)
import qualified Data.Text.Lazy                  as LT (Text)
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

-- | Sum of a type-level list of functors
data SumF :: [* -> *] -> * -> * where
  Here  :: !(f a)       -> SumF (f ': fs) a
  There :: !(SumF fs a) -> SumF (f ': fs) a

deriving instance (Functor f, Functor (SumF fs)) => Functor (SumF (f ': fs))

instance Functor (SumF '[]) where
  fmap _ impossible = case impossible of {}

-- | Product of a tyoe-level list of functors
class ProductFunctor (fs :: [* -> *]) where
  data ProductF fs :: * -> *

instance ProductFunctor '[] where
  data ProductF '[] a = ProductNil deriving Functor

instance ProductFunctor (f ': fs) where
  data ProductF (f ': fs) a = Functor f => Some (f a) (ProductF fs a)

deriving instance Functor (ProductF fs) => Functor (ProductF (f ': fs))


-- | Right-associative tuple type-constructor
type a ::: b = (a, b)
infixr 0 :::

-- | Right-associative tuple data-constructor
(+:) :: a -> b -> (a,b)
(+:) a b = (a, b)
infixr 0 +:

(*:) :: (Functor f, Functor (ProductF fs)) => (a -> f a) -> (b -> ProductF fs b) -> (a, b) -> ProductF (f ': fs) (a, b)
(*:) f g (a,b) = Some ((,b) <$> f a) ((a,) <$> g b)
infixr 0 *:

($:) :: (Functor f, Functor g) => (a -> f a) -> (b -> g b) -> (a, b) -> ProductF '[f, g] (a, b)
($:) f g (a,b) = Some ((,b) <$> f a) $ Some ((a,) <$> g b) ProductNil


-- | A constraint @Injectable f g@ expresses that @f@ is subsumed by @g@,
-- i.e. @f@ can be used to construct elements in @g@.
class Injectable (f :: * -> *) (fs :: [* -> *]) where
  inj :: f a -> SumF fs a

instance Functor f => Injectable f (f ': fs) where
  inj = Here

instance {-# OVERLAPPABLE #-} Injectable f fs => Injectable f (g ': fs) where
  inj = There . inj


class Outjectable (f :: * -> *) (fs :: [* -> *]) where
  outj :: SumF fs a -> Maybe (f a)

instance Outjectable f (f ': fs) where
  outj (Here a) = Just a
  outj _        = Nothing

instance {-# OVERLAPPABLE #-} Outjectable f fs => Outjectable f (g ': fs) where
  outj (There a) = outj a
  outj _         = Nothing


class (Injectable f fs, Functor (SumF fs))
  => (f :: * -> *) :<: (fs :: [* -> *])
instance (Injectable f fs, Outjectable f fs, Functor (SumF fs))
  => (f :<: fs)


-- | Functors @f@ and @g@ are paired when they can annihilate each other
class (Monad m, Functor f, Functor g) => PairingM f g m | f -> g where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

instance (Monad m) => PairingM Identity Identity m where
  pairM f (Identity a) (Identity b) = f a b

class PairingList m (f :: [* -> *]) (g :: [* -> *]) | f -> g where
  pairList :: (a -> b -> m r) -> (ProductF f) a -> (SumF g) b -> m r

instance PairingList m '[] '[] where
  pairList _ _ x = case x of {}

instance (PairingList m fs gs, PairingM f g m) => PairingList m (f ': fs) (g ': gs) where
  pairList p (Some a _) (Here x)  = pairM p a x
  pairList p (Some _ b) (There x) = pairList p b x


interpret :: Functor (ProductF f) => Functor (SumF g) => Monad m => PairingList m f g
          => (a -> b -> m r) -> Cofree (ProductF f) a -> FreeT (SumF g) m b -> m r
interpret p eval program = do
  let a = extract eval
  b <- runFreeT program
  case b of
    Pure x  -> p a x
    Free gs -> pairList (interpret p) (unwrap eval) gs


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
