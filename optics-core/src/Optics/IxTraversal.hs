{-# LANGUAGE DataKinds #-}
module Optics.IxTraversal
  ( A_Traversal
  , IxTraversal
  , IxTraversal'
  , TraversableWithIndex(..)
  , toIxTraversal
  , ixTraversalVL
  , conjoinedTraversal
  , itraverseOf
  , iforOf
  , itraversed
  , module Optics.Optic
  ) where

import Optics.Internal.Indexed
import Optics.Internal.IxTraversal
import Optics.Internal.Profunctor
import Optics.Internal.Optic
import Optics.Optic

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
  :: (Is k A_Traversal, Applicative f, (is `HasSingleIndex` i) "itraverseOf" 1)
  => Optic k is s t a b
  -> (i -> a -> f b) -> s -> f t
itraverseOf o f = runIxStar (getOptic (toIxTraversal o) (IxStar f)) id
{-# INLINE itraverseOf #-}

-- | A version of 'itraverseOf' with the arguments flipped.
iforOf
  :: (Is k A_Traversal, Applicative f, (is `HasSingleIndex` i) "iforOf" 1)
  => Optic k is s t a b
  -> s -> (i -> a -> f b) -> f t
iforOf = flip . itraverseOf
{-# INLINE iforOf #-}

-- | Indexed traversal via the 'TraversableWithIndex' class.
--
-- >>> iover (itraversed <%> itraversed) (,) ["ab", "cd"]
-- [[((0,0),'a'),((0,1),'b')],[((1,0),'c'),((1,1),'d')]]
--
itraversed
  :: TraversableWithIndex i f
  => IxTraversal i (f a) (f b) a b
itraversed = Optic itraversed__
{-# INLINE itraversed #-}
