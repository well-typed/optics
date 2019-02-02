module Optics.Internal.IxTraversal where

import Optics.Internal.Fold
import Optics.Internal.Indexed
import Optics.Internal.IxFold
import Optics.Internal.IxSetter
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Setter

-- | Internal implementation of 'itraversed'.
itraversed__
  :: (Traversing p, TraversableWithIndex i f)
  => Optic__ p j (i -> j) (f a) (f b) a b
itraversed__ = conjoinedTraversal__ traverse itraverse
{-# INLINE [0] itraversed__ #-}

-- Because itraversed__ inlines late, GHC needs rewrite rules for all cases in
-- order to generate optimal code for each of them. The ones that rewrite
-- traversal into a traversal correspond to an early inline.

{-# RULES

"itraversed__ -> wander traverse"
  forall (o :: Star g j a b). itraversed__ o = wander traverse (reStar o)
    :: TraversableWithIndex i f => Star g (i -> j) (f a) (f b)

"itraversed__ -> folded__"
  forall (o :: Forget r j a b). itraversed__ o = folded__ (reForget o)
    :: FoldableWithIndex i f => Forget r (i -> j) (f a) (f b)

"itraversed__ -> mapped__"
  forall (o :: FunArrow j a b). itraversed__ o = mapped__ (reFunArrow o)
    :: FunctorWithIndex i f => FunArrow (i -> j) (f a) (f b)

"itraversed__ -> itraverse"
  forall (o :: IxStar g j a b). itraversed__ o = iwander itraverse o
    :: TraversableWithIndex i f => IxStar g (i -> j) (f a) (f b)

"itraversed__ -> ifolded__"
  forall (o :: IxForget r j a b). itraversed__ o = ifolded__ o
    :: FoldableWithIndex i f => IxForget r (i -> j) (f a) (f b)

"itraversed__ -> imapped__"
  forall (o :: IxFunArrow j a b). itraversed__ o = imapped__ o
    :: FunctorWithIndex i f => IxFunArrow (i -> j) (f a) (f b)

#-}

-- | Internal implementation of 'conjoinedTraversal'.
conjoinedTraversal__
  :: Traversing p
  => (forall f. Applicative f => (     a -> f b) -> s -> f t)
  -> (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> Optic__ p j (i -> j) s t a b
conjoinedTraversal__ f g = conjoined (wander f) (iwander g)
{-# INLINE conjoinedTraversal__ #-}
