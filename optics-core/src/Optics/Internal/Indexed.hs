{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Optics.Internal.Indexed where

import Data.Foldable
import Data.Functor.Identity
import Data.Monoid

import Optics.Internal.Utils

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
