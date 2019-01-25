module Optics.Internal.IxTraversal where

import Optics.Internal.Indexed
import Optics.Internal.IxFold
import Optics.Internal.IxSetter
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Internal implementation of 'itraversed'.
itraversed__
  :: (Traversing p, TraversableWithIndex i f)
  => Optic__ p j (i -> j) (f a) (f b) a b
itraversed__ = conjoinedTraversal__ traverse itraverse
{-# INLINE [0] itraversed__ #-}

{-# RULES

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
