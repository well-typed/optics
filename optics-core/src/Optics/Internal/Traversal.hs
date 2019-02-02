module Optics.Internal.Traversal where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Internal implementation of 'traversed'.
traversed__
  :: (Traversing p, Traversable f)
  => Optic__ p i i (f a) (f b) a b
traversed__ = wander traverse
{-# INLINE traversed__ #-}
