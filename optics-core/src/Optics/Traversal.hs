module Optics.Traversal
  (
  -- * Formation
    A_Traversal
  , Traversal
  , Traversal'
  -- * Introduction
  , traversed
  , toTraversal
  -- * Elimination
  , traverseOf
  , forOf
  -- * van Laarhoven encoding
  , TraversalVL
  , TraversalVL'
  , traversalVL
  -- * Travelsals
  , elementsOf
  , elementOf
  -- * Re-exports
  , module Optics.Optic
  )
  where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Traversal
import Optics.Internal.Utils
import Optics.Optic

-- | Type synonym for a type-modifying traversal.
type Traversal s t a b = Optic A_Traversal NoIx s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal' s a = Optic' A_Traversal NoIx s a

-- | Type synonym for a type-modifying van Laarhoven traversal.
type TraversalVL s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven traversal.
type TraversalVL' s a = TraversalVL s s a a

-- | Explicitly cast an optic to a traversal.
toTraversal
  :: Is k A_Traversal
  => Optic k is s t a b
  -> Optic A_Traversal is s t a b
toTraversal = castOptic
{-# INLINE toTraversal #-}

-- | Build a traversal from the van Laarhoven representation.
--
-- @
-- 'traversalVL' '.' 'traverseOf' ≡ 'id'
-- 'traverseOf' '.' 'traversalVL' ≡ 'id'
-- @
traversalVL :: TraversalVL s t a b -> Traversal s t a b
traversalVL t = Optic (wander t)
{-# INLINE traversalVL #-}

-- | Traversal via the 'Traversable' class.
traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = Optic traversed__
{-# INLINE traversed #-}

-- | Map each element of a structure targeted by a 'Traversal', evaluate these
-- actions from left to right, and collect the results.
traverseOf
  :: (Is k A_Traversal, Applicative f)
  => Optic k is s t a b
  -> (a -> f b) -> s -> f t
traverseOf o = runStar #. getOptic (toTraversal o) .# Star
{-# INLINE traverseOf #-}

-- | A version of 'traverseOf' with the arguments flipped.
forOf
  :: (Is k A_Traversal, Applicative f)
  => Optic k is s t a b
  -> s -> (a -> f b) -> f t
forOf = flip . traverseOf
{-# INLINE forOf #-}

----------------------------------------
-- Traversals

-- Traverse selected elements of a 'Traversal' where their ordinal positions
-- match a predicate.
elementsOf :: Traversal s t a a -> (Int -> Bool) -> Traversal s t a a
elementsOf o p = traversalVL $ \f ->
  indexing (traverseOf o) $ \i a -> if p i then f a else pure a
{-# INLINE elementsOf #-}

-- | Traverse the /nth/ element of a 'Traversal' if it exists.
elementOf :: Traversal s t a a -> Int -> Traversal s t a a
elementOf o i = elementsOf o (== i)
{-# INLINE elementOf #-}
