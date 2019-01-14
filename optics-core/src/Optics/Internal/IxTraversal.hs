module Optics.Internal.IxTraversal where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying indexed traversal.
type IxTraversal i o s t a b = Optic A_Traversal i o s t a b

-- | Type synonym for a type-preserving indexed traversal.
type IxTraversal' i o s a = Optic' A_Traversal i o s a

-- | Explicitly cast an optic to an indexed traversal.
toIxTraversal
  :: Is k A_Traversal
  => Optic k i o s t a b
  -> IxTraversal i o s t a b
toIxTraversal = sub
{-# INLINE toIxTraversal #-}

-- | Build an indexed traversal from the van Laarhoven representation.
vlIxTraversal
  :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IxTraversal j (i -> j) s t a b
vlIxTraversal t = Optic (iwander t)
{-# INLINE vlIxTraversal #-}

-- | Indexed traversal via the 'TraversableWithIndex' class.
itraversed
  :: TraversableWithIndex i t
  => IxTraversal j (i -> j) (t a) (t b) a b
itraversed = vlIxTraversal itraverse
{-# INLINE itraversed #-}

itraverseOf
  :: (CheckIndices i o, Is k A_Traversal)
  => Optic k i o s t a b
  -> (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
itraverseOf o f = runIxStar (getOptic (toIxTraversal o) (IxStar f)) id
{-# INLINE itraverseOf #-}

----------------------------------------

-- | Flatten indices obtained from two indexed optics.
icompose
  :: (i -> j -> ix)
  -> IxTraversal (i -> j -> r) (ix -> r) a b a b
icompose f = Optic (ixcontramap (\g i j -> g (f i j)))
{-# INLINE icompose #-}

-- | Flatten indices obtained from three indexed optics.
icompose3
  :: (i -> j -> k -> ix)
  -> IxTraversal (i -> j -> k -> r) (ix -> r) a b a b
icompose3 f = Optic (ixcontramap (\g i j k -> g (f i j k)))
{-# INLINE icompose3 #-}

-- | Flatten indices obtained from four indexed optics.
icompose4
  :: (i -> j -> k -> l -> ix)
  -> IxTraversal (i -> j -> k -> l -> r) (ix -> r) a b a b
icompose4 f = Optic (ixcontramap (\g i j k l -> g (f i j k l)))
{-# INLINE icompose4 #-}

-- | Flatten indices obtained from five indexed optics.
icompose5
  :: (i -> j -> k -> l -> m -> ix)
  -> IxTraversal (i -> j -> k -> l -> m -> r) (ix -> r) a b a b
icompose5 f = Optic (ixcontramap (\g i j k l m -> g (f i j k l m)))
{-# INLINE icompose5 #-}
