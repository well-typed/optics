{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Optics.Indexed
  (
  -- * Indexed optics
    module Optics.IxTraversal
  , module Optics.IxFold
  , module Optics.IxSetter

  -- * Index composition
  , (<%>)
  , (%>)
  , (<%)
  , reindex
  , icompose
  , icompose3
  , icompose4
  , icompose5

  -- * Index removal
  , NoIxOptic(..)

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

import Optics.Fold
import Optics.IxFold
import Optics.IxSetter
import Optics.IxTraversal
import Optics.Setter
import Optics.Traversal

-- | Compose two indexed optics. Their indices are composed as a pair.
--
-- >>> itoListOf (ifolded <%> ifolded) ["foo", "bar"]
-- [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
infixr 9 <%>
(<%>)
  :: ( m ~ Join k l, n ~ Join A_Traversal m
     , Is k m, Is l m, Is m n, Is A_Traversal n
     , Append is js ~ '[i, j])
  => Optic k is        s t u v
  -> Optic l js        u v a b
  -> Optic n '[(i, j)] s t a b
o <%> o' = icompose (,) (o % o')
{-# INLINE (<%>) #-}

-- | Compose two optics and preserve indices of the right one.
--
-- >>> itoListOf (ifolded %> ifolded) ["foo", "bar"]
-- [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]
--
infixr 9 %>
(%>)
  :: (Is k m, Is l m, m ~ Join k l, NoIxOptic k s t u v)
  => Optic k is s t u v
  -> Optic l js u v a b
  -> Optic m js s t a b
o %> o' = noIx o % o'
{-# INLINE (%>) #-}

-- | Compose two optics and preserve indices of the left one.
--
-- >>> itoListOf (ifolded <% ifolded) ["foo", "bar"]
-- [(0,'f'),(0,'o'),(0,'o'),(1,'b'),(1,'a'),(1,'r')]
--
infixr 9 <%
(<%)
 :: (Is l m, Is k m, m ~ Join k l, NoIxOptic l u v a b)
 => Optic k is s t u v
 -> Optic l js u v a b
 -> Optic m is s t a b
o <% o' = o % noIx o'
{-# INLINE (<%) #-}

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

----------------------------------------
-- NoIxOptic

class NoIxOptic k s t a b where
  -- | Convert an indexed optic to its unindexed equivalent.
  noIx :: Optic k is s t a b -> Optic k NoIx s t a b

instance NoIxOptic A_Traversal s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = traversalVL (traverseOf o)
  {-# INLINE noIx #-}

instance (s ~ t, a ~ b) => NoIxOptic A_Fold s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = foldVL (traverseOf_ o)
  {-# INLINE noIx #-}

instance NoIxOptic A_Setter s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = sets (over o)
  {-# INLINE noIx #-}

----------------------------------------
-- Internal

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

-- $setup
-- >>> import Optics.Core
