module Optics.Internal.Setter where

import Optics.Internal.Profunctor
import Optics.Internal.Optic

-- | Internal implementation of 'mapped'.
mapped__
  :: (Mapping p, Functor f)
  => Optic__ p i i (f a) (f b) a b
mapped__ = roam fmap
{-# INLINE mapped__ #-}
