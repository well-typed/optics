{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of traversals.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Traversal where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Fold
import Optics.Internal.Setter

-- | Internal implementation of 'Optics.Traversal.traversed'.
traversed__
  :: (Traversing p, Traversable f)
  => Optic__ p i ci i ci (f a) (f b) a b
traversed__ = wander traverse
{-# INLINE [0] traversed__ #-}

-- Because traversed__ inlines late, GHC needs rewrite rules for all cases in
-- order to generate optimal code for each of them. The one that rewrites
-- traversal into a traversal correspond to an early inline.

{-# RULES

"traversed__ -> wander traverse"
  forall (o :: Star g i ci a b). traversed__ o = wander traverse o
    :: Traversable f => Star g i ci (f a) (f b)

"traversed__ -> folded__"
  forall (o :: Forget r i ci a b). traversed__ o = folded__ o
    :: Foldable f => Forget r i ci (f a) (f b)

"traversed__ -> mapped__"
  forall (o :: FunArrow i ci a b). traversed__ o = mapped__ o
    :: Functor f => FunArrow i ci (f a) (f b)

#-}
