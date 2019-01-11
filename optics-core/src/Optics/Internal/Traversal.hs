module Optics.Internal.Traversal where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a type-modifying traversal.
type Traversal i s t a b = Optic A_Traversal i i s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal' i s a = Optic' A_Traversal i i s a

-- | Type synonym for a type-modifying van Laarhoven traversal.
type TraversalVL s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven traversal.
type TraversalVL' s a = TraversalVL s s a a

-- | Explicitly cast an optic to a traversal.
toTraversal
  :: Is k A_Traversal
  => Optic k i o s t a b
  -> Optic A_Traversal i o s t a b
toTraversal = sub
{-# INLINE toTraversal #-}

-- | Build a traversal from the van Laarhoven representation.
traversalVL :: TraversalVL s t a b -> Traversal i s t a b
traversalVL t = Optic (wander t)
{-# INLINE traversalVL #-}

-- | Traversal via the 'Traversable' class.
traversed :: Traversable t => Traversal i (t a) (t b) a b
traversed = traversalVL traverse
{-# INLINE traversed #-}

-- | Map each element of a structure targeted by a 'Traversal', evaluate these
-- actions from left to right, and collect the results.
traverseOf
  :: (Is k A_Traversal, Applicative f) => Optic k i o s t a b
  -> (a -> f b) -> s -> f t
traverseOf o = runStar #. getOptic (toTraversal o) .# Star
{-# INLINE traverseOf #-}

----------------------------------------

-- Traverse selected elements of a 'Traversal' where their ordinal positions
-- match a predicate.
elementsOf :: Traversal i s t a a -> (Int -> Bool) -> Traversal i s t a a
elementsOf o p = traversalVL $ \f ->
  indexing (traverseOf o) $ \i a -> if p i then f a else pure a
{-# INLINE elementsOf #-}

-- | Traverse the /nth/ element of a 'Traversal' if it exists.
elementOf :: Traversal i s t a a -> Int -> Traversal i s t a a
elementOf o i = elementsOf o (== i)
{-# INLINE elementOf #-}
