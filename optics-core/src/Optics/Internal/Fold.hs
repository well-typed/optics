module Optics.Internal.Fold where

import Data.Functor

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Internal implementation of 'foldVL'.
foldVL__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (a -> f r) -> s -> f ())
  -> Optic__ p i i s t a b
foldVL__ f = rphantom . wander f . rphantom
{-# INLINE foldVL__ #-}

-- | Internal implementation of 'foldring'.
foldring__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (a -> f r -> f r) -> f r -> s -> f r)
  -> Optic__ p i i s t a b
foldring__ fr = foldVL__ $ \f -> void . fr (\a -> (f a *>)) (pure v)
  where
    v = error "foldring__: value used"
{-# INLINE foldring__ #-}
