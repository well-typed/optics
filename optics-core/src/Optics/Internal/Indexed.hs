{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.Internal.Indexed where

import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Optics.Internal.Utils

-- | Generate sensible error messages in case a user tries to use regular optic
-- as an indexed one or doesn't call appropriate icompose function to flatten
-- the indices before trying to use an indexed optic.
class o ~ (i -> i) => CheckIndices i o

instance CheckIndices i (i -> i)

instance
  ( TypeError ('Text "Regular optic cannot be used as an indexed one")
  , i ~ (i -> i)
  ) => CheckIndices i i

instance
  ( TypeError ('Text "Precompose with icompose to flatten indices")
  , (a -> b -> i) ~ (i -> i)
  ) => CheckIndices i (a -> b -> i)

instance
  ( TypeError ('Text "Precompose with icompose3 to flatten indices")
  , (a -> b -> c -> i) ~ (i -> i)
  ) => CheckIndices i (a -> b -> c -> i)

instance
  ( TypeError ('Text "Precompose with icompose4 to flatten indices")
  , (a -> b -> c -> d -> i) ~ (i -> i)
  ) => CheckIndices i (a -> b -> c -> d -> i)

instance
  ( TypeError ('Text "Precompose with icompose5 to flatten indices")
  , (a -> b -> c -> d -> e -> i) ~ (i -> i)
  ) => CheckIndices i (a -> b -> c -> d -> e -> i)

instance {-# OVERLAPPABLE #-}
  ( TypeError ('Text "Use icompose* variants to flatten indices")
  , o ~ (i -> i)
  ) => CheckIndices i o

----------------------------------------

newtype Indexing f a = Indexing { runIndexing :: Int -> (Int, f a) }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    (j, x) -> (j, fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> (i, pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    (j, ff) -> case ma j of
       ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

indexing
  :: ((a -> Indexing f b) -> s -> Indexing f t)
  -> ((Int -> a -> f b) -> s -> f t)
indexing l iafb s =
  snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, iafb i a))) s) 0
{-# INLINE indexing #-}

----------------------------------------

class Functor f => FunctorWithIndex i f | f -> i where
  imap :: (i -> a -> b) -> f a -> f b
  default imap
    :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
  imap f = runIdentity #. itraverse (\i -> Identity #. f i)
  {-# INLINE imap #-}

class (FunctorWithIndex i f, Foldable f
      ) => FoldableWithIndex i f | f -> i where
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m
  default ifoldMap
    :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
  ifoldMap f = (fold .# runIdentity) . itraverse (\i -> Identity #. f i)
  {-# INLINE ifoldMap #-}

  ifoldr :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr iabb b0 = (\e -> appEndo e b0) . ifoldMap (\i -> Endo #. iabb i)
  {-# INLINE ifoldr #-}

  ifoldl' :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl' ibab b0 s = ifoldr (\i a bb b -> bb $! ibab i b a) id s b0
  {-# INLINE ifoldl' #-}


class (FoldableWithIndex i t, Traversable t
      ) => TraversableWithIndex i t | t -> i where
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

----------------------------------------

instance FunctorWithIndex Int []
instance FoldableWithIndex Int []
instance TraversableWithIndex Int [] where
  itraverse = indexing traverse
  {-# INLINE itraverse #-}

-- TODO: more instances

----------------------------------------

itraverse_ :: (FoldableWithIndex i t, Applicative f) => (i -> a -> f b) -> t a -> f ()
itraverse_ f = ifoldr (\i -> (*>) . f i) (pure ())
{-# INLINE itraverse_ #-}
