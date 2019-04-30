{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of setters.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Setter where

import Optics.Internal.Profunctor
import Optics.Internal.Optic

-- | Internal implementation of 'Optics.Setter.mapped'.
mapped__
  :: (Mapping p, Functor f)
  => Optic__ p l i l i (f a) (f b) a b
mapped__ = roam fmap
{-# INLINE mapped__ #-}
