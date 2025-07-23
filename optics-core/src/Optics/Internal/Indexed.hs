{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of indexed optics.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Indexed where

import Data.Kind (Type)
import GHC.TypeLits

import Data.Profunctor.Indexed
import Optics.Internal.Optic

-- | Show useful error message when a function expects optics without indices.
class is ~ NoIx => AcceptsEmptyIndices (f :: Symbol) (is :: IxList)

instance
  ( TypeError
    (Text "‘" :<>: Text f :<>: Text "’ accepts only optics with no indices")
  , (x : xs) ~ NoIx
  ) => AcceptsEmptyIndices f (x : xs)

instance AcceptsEmptyIndices f '[]

-- | Check whether a list of indices is not empty and generate sensible error
-- message if it's not.
class NonEmptyIndices (is :: IxList)

instance
  ( TypeError
    (Text "Indexed optic is expected")
  ) => NonEmptyIndices '[]

instance NonEmptyIndices (x : xs)

-- | Generate sensible error messages in case a user tries to pass either an
-- unindexed optic or indexed optic with unflattened indices where indexed optic
-- with a single index is expected.
class is ~ '[i] => HasSingleIndex (is :: IxList) (i :: Type)

instance HasSingleIndex '[i] i

instance
  ( TypeError
    (Text "Indexed optic is expected")
  , '[] ~ '[i]
  ) => HasSingleIndex '[] i

instance
  ( TypeError
    (Text "Use (<%>) or icompose to combine indices of type "
     :<>: ShowTypes is)
  , is ~ '[i1, i2]
  , is ~ '[i]
  ) => HasSingleIndex '[i1, i2] i

instance
  ( TypeError
    (Text "Use icompose3 to combine indices of type "
     :<>: ShowTypes is)
  , is ~ '[i1, i2, i3]
  , is ~ '[i]
  ) => HasSingleIndex [i1, i2, i3] i

instance
  ( TypeError
    (Text "Use icompose4 to combine indices of type "
     :<>: ShowTypes is)
  , is ~ '[i1, i2, i3, i4]
  , is ~ '[i]
  ) => HasSingleIndex '[i1, i2, i3, i4] i

instance
  ( TypeError
    (Text "Use icompose5 to flatten indices of type "
     :<>: ShowTypes is)
  , is ~ '[i1, i2, i3, i4, i5]
  , is ~ '[i]
  ) => HasSingleIndex '[i1, i2, i3, i4, i5] i

instance
  ( TypeError
    (Text "Use icomposeN to flatten indices of type "
     :<>: ShowTypes is)
  , is ~ (i1 : i2 : i3 : i4 : i5 : i6 : is')
  , is ~ '[i]
  ) => HasSingleIndex (i1 : i2 : i3 : i4 : i5 : i6 : is') i

----------------------------------------
-- Helpers for HasSingleIndex

type family ShowTypes (types :: [Type]) :: ErrorMessage where
  ShowTypes '[i]     = QuoteType i
  ShowTypes '[i, j]  = QuoteType i :<>: Text " and " :<>: QuoteType j
  ShowTypes (i : is) = QuoteType i :<>: Text ", " :<>: ShowTypes is

----------------------------------------

data IntT f a = IntT {-# UNPACK #-} !Int (f a)

unIntT :: IntT f a -> f a
unIntT (IntT _ fa) = fa

newtype Indexing f a = Indexing { runIndexing :: Int -> IntT f a }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    IntT j x -> IntT j (fmap f x)

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> IntT i (pure x)
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    IntT j ff -> case ma j of
       IntT k fa -> IntT k (ff <*> fa)

-- | Index a traversal by position of visited elements.
indexing
  :: ((a -> Indexing f b) -> s -> Indexing f t)
  -> ((Int -> a -> f b) -> s -> f t)
indexing l iafb s =
  unIntT $ runIndexing (l (\a -> Indexing (\i -> IntT (i + 1) (iafb i a))) s) 0

----------------------------------------

-- | Construct a conjoined indexed optic that provides a separate code path when
-- used without indices. Useful for defining indexed optics that are as
-- efficient as their unindexed equivalents when used without indices.
--
-- /Note:/ @'conjoined' f g@ is well-defined if and only if @f ≡
-- 'Optics.Indexed.Core.noIx' g@.
conjoined
  :: is `HasSingleIndex` i
  => Optic k NoIx s t a b
  -> Optic k is   s t a b
  -> Optic k is   s t a b
conjoined (Optic f) (Optic g) = Optic (conjoined__ f g)
