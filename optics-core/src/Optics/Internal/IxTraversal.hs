{-# LANGUAGE TypeApplications #-}
module Optics.Internal.IxTraversal where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Optic.TypeLevel
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying indexed traversal.
type IxTraversal i s t a b = Optic A_Traversal '[i] s t a b

-- | Type synonym for a type-preserving indexed traversal.
type IxTraversal' i s a = Optic' A_Traversal '[i] s a

-- | Explicitly cast an optic to an indexed traversal.
toIxTraversal
  :: Is k A_Traversal
  => Optic k '[i] s t a b
  -> IxTraversal i s t a b
toIxTraversal = sub
{-# INLINE toIxTraversal #-}

-- | Build an indexed traversal from the van Laarhoven representation.
vlIxTraversal
  :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IxTraversal i s t a b
vlIxTraversal t = Optic (iwander t)
{-# INLINE vlIxTraversal #-}

-- | Indexed traversal via the 'TraversableWithIndex' class.
--
-- >>> iover (icompose (,) $ itraversed % itraversed) (,) ["ab", "cd"]
-- [[((0,0),'a'),((0,1),'b')],[((1,0),'c'),((1,1),'d')]]
--
itraversed
  :: TraversableWithIndex i t
  => IxTraversal i (t a) (t b) a b
itraversed = vlIxTraversal itraverse
{-# INLINE itraversed #-}

itraverseOf
  :: (CheckIndices i is, Is k A_Traversal)
  => Optic k is s t a b
  -> (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
itraverseOf o f = runIxStar (getOptic (toIxTraversal o) (IxStar f)) id
{-# INLINE itraverseOf #-}

----------------------------------------

-- | Flatten indices obtained from two indexed optics.
icompose
  :: (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => (i -> j -> ix)
  -> Optic k '[i, j] s t a b
  -> Optic l '[ix] s t a b
icompose = icomposeN

-- | Flatten indices obtained from three indexed optics.
icompose3
  :: (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => (i1 -> i2 -> i3 -> ix)
  -> Optic k '[i1, i2, i3] s t a b
  -> Optic l '[ix] s t a b
icompose3 = icomposeN
{-# INLINE icompose3 #-}

-- | Flatten indices obtained from four indexed optics.
icompose4
  :: (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => (i1 -> i2 -> i3 -> i4 -> ix)
  -> Optic k '[i1, i2, i3, i4] s t a b
  -> Optic l '[ix] s t a b
icompose4 = icomposeN
{-# INLINE icompose4 #-}

-- | Flatten indices obtained from five indexed optics.
icompose5
  :: (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => (i1 -> i2 -> i3 -> i4 -> i5 -> ix)
  -> Optic k '[i1, i2, i3, i4, i5] s t a b
  -> Optic l '[ix] s t a b
icompose5 = icomposeN
{-# INLINE icompose5 #-}

-- Implementation of icompose*
icomposeN
  :: forall k l i is s t a b. (Is A_Traversal l, Is k l, Join A_Traversal k ~ l, CurryCompose is)
  => Curry is i
  -> Optic k is s t a b
  -> Optic l '[i] s t a b
icomposeN f o = Optic (implies' (ixcontramap (\ij -> composeN @is ij f)) . getOptic (sub @k @l o))
  where
    implies' :: forall p ij. Optic_ A_Traversal p (Curry is ij) (i -> ij) s t s t -> Optic_ l p (Curry is ij) (i -> ij) s t s t
    implies' x = implies (IsProxy :: IsProxy A_Traversal l p) x
{-# INLINE icomposeN #-}

-- $setup
-- >>> import Optics
