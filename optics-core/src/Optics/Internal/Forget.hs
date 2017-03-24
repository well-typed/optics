-- | Taken from the profunctors package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Forget where

import Data.Monoid
import Data.Traversable

import Optics.Internal.Profunctor

data Forget r a b =
  Forget { runForget :: a -> r }

instance Functor (Forget r a) where
  fmap _f = Forget . runForget
  {-# INLINE fmap #-}

instance Profunctor (Forget r) where
  dimap ca _bd = Forget . (. ca) . runForget
  {-# INLINE dimap #-}

instance Strong (Forget r) where
  first'  = Forget . (. fst) . runForget
  {-# INLINE first' #-}
  second' = Forget . (. snd) . runForget
  {-# INLINE second' #-}

instance OutPhantom (Forget r) where
  ocoerce = Forget . runForget
  {-# INLINE ocoerce #-}

instance Monoid r => Choice (Forget r) where
  left'  = Forget . (\ ar -> either ar (const mempty)) . runForget
  {-# INLINE left' #-}
  right' = Forget . (\ ar -> either (const mempty) ar) . runForget
  {-# INLINE right' #-}

instance Monoid r => Monoidal (Forget r) where
  empty = Forget (const mempty)
  {-# INLINE empty #-}
  par (Forget ar) (Forget cr) = Forget (\ (a, c) -> ar a <> cr c)
  {-# INLINE par #-}

instance Monoid r => Wandering (Forget r) where
  wander = Forget . foldMapDefault . runForget
  {-# INLINE wander #-}
