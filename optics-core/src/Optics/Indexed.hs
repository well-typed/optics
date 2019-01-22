{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Optics.Indexed
  (
  -- * Indexed optics
    module Optics.IxTraversal
  , module Optics.IxFold
  , module Optics.IxSetter

  -- * Index composition
  , reindex
  , icompose
  , icompose3
  , icompose4
  , icompose5

  , conjoined'

  -- * Functors with index
  , FunctorWithIndex (..)
  -- ** Foldable with index
  , FoldableWithIndex (..)
  , itraverse_
  , ifor_
  -- ** Traversable with index
  , TraversableWithIndex (..)
  , ifor
  ) where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Optic.TypeLevel
import Optics.Internal.Profunctor

import Optics.IxTraversal
import Optics.IxFold
import Optics.IxSetter

-- | Remap the index.
--
-- >>> itoListOf (reindex succ ifolded) "foo"
-- [(1,'f'),(2,'o'),(3,'o')]
--
reindex
  :: (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => (i -> j)
  -> Optic k '[i] s t a b
  -> Optic l '[j] s t a b
reindex = icomposeN
{-# INLINE reindex #-}

-- | Flatten indices obtained from two indexed optics.
icompose
  :: (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => (i -> j -> ix)
  -> Optic k '[i, j] s t a b
  -> Optic l '[ix] s t a b
icompose = icomposeN
{-# INLINE icompose #-}

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
  :: forall k l i is s t a b
  . (Is A_Traversal l, Is k l, Join A_Traversal k ~ l, CurryCompose is)
  => Curry is i
  -> Optic k is s t a b
  -> Optic l '[i] s t a b
icomposeN f o = Optic $
    implies' (ixcontramap (\ij -> composeN @is ij f))
  . getOptic (castOptic @k @l o)
  where
    implies'
      :: forall p ij
      .  Optic_ A_Traversal p (Curry is ij) (i -> ij) s t s t
      -> Optic_ l           p (Curry is ij) (i -> ij) s t s t
    implies' x = implies (IsProxy :: IsProxy A_Traversal l p) x
{-# INLINE icomposeN #-}

-- | Proper documentation TBW
--
-- @conjoined' a b@ behaves like @b@ when @a@ behaves like @'noIx' b@,
-- otherwise behaviour is undefined.
--
conjoined'
  :: forall k l is s t a b. (Is A_Traversal l, Is k l, Join A_Traversal k ~ l)
  => Optic k NoIx s t a b
  -> Optic k is   s t a b
  -> Optic l is   s t a b
conjoined' noix withix =
  case (castOptic noix :: Optic l NoIx s t a b, castOptic withix :: Optic l is s t a b) of
    (Optic p, Optic q) -> Optic $ impl conjoined2 p q
  where
    impl :: forall p j
      . (Constraints A_Traversal p
          => Optic__ p j           j  s t a b
          -> Optic__ p j (Curry is j) s t a b
          -> Optic__ p j (Curry is j) s t a b)
      -> (Constraints l p
          => Optic__ p j           j  s t a b
          -> Optic__ p j (Curry is j) s t a b
          -> Optic__ p j (Curry is j) s t a b)
    impl x = implies (IsProxy :: IsProxy A_Traversal l p) x

-- $setup
-- >>> import Optics
