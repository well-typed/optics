module Optics.Internal.IxTraversal where

import Optics.Internal.Indexed
import Optics.Internal.IxFold
import Optics.Internal.IxSetter
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying indexed traversal.
type IxTraversal i s t a b = Optic A_Traversal (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed traversal.
type IxTraversal' i s a = Optic' A_Traversal (WithIx i) s a

-- | Explicitly cast an optic to an indexed traversal.
toIxTraversal
  :: Is k A_Traversal
  => Optic k (WithIx i) s t a b
  -> IxTraversal i s t a b
toIxTraversal = castOptic
{-# INLINE toIxTraversal #-}

-- | Build an indexed traversal from the van Laarhoven representation.
--
-- @
-- 'ixTraversalVL' '.' 'itraverseOf' ≡ 'id'
-- 'itraverseOf' '.' 'ixTraversalVL' ≡ 'id'
-- @
ixTraversalVL
  :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IxTraversal i s t a b
ixTraversalVL t = Optic (iwander t)
{-# INLINE ixTraversalVL #-}

-- | Build an indexed traversal from the van Laarhoven representation of both
-- its unindexed and indexed version.
--
-- Appropriate version of the traversal will be automatically picked for maximum
-- efficiency depending on whether it is used as indexed or regular one.
--
-- @
-- 'traverseOf'  ('conjoinedTraversal' f g) ≡ 'traverseOf'  ('traversalVL' f)
-- 'itraverseOf' ('conjoinedTraversal' f g) ≡ 'itraverseOf' ('ixTraversalVL' g)
-- @
conjoinedTraversal
  :: (forall f. Applicative f => (     a -> f b) -> s -> f t)
  -> (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IxTraversal i s t a b
conjoinedTraversal f g = Optic (conjoinedTraversal__ f g)
{-# INLINE conjoinedTraversal #-}

----------------------------------------

itraverseOf
  :: (Is k A_Traversal, CheckIndices i is, Applicative f)
  => Optic k is s t a b
  -> (i -> a -> f b) -> s -> f t
itraverseOf o f = runIxStar (getOptic (toIxTraversal o) (IxStar f)) id
{-# INLINE itraverseOf #-}

-- | A version of 'itraverseOf' with the arguments flipped.
iforOf
  :: (Is k A_Traversal, CheckIndices i is, Applicative f)
  => Optic k is s t a b
  -> s -> (i -> a -> f b) -> f t
iforOf = flip . itraverseOf
{-# INLINE iforOf #-}

-- | Indexed traversal via the 'TraversableWithIndex' class.
--
-- >>> iover (icompose (,) $ itraversed % itraversed) (,) ["ab", "cd"]
-- [[((0,0),'a'),((0,1),'b')],[((1,0),'c'),((1,1),'d')]]
--
itraversed
  :: TraversableWithIndex i f
  => IxTraversal i (f a) (f b) a b
itraversed = Optic itraversed__
{-# INLINE itraversed #-}

----------------------------------------
-- Internal implementations

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
