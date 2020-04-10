{-# LANGUAGE DataKinds #-}
-- |
-- Module: Optics.Indexed.Core
-- Description: Core definitions for indexed optics.
--
-- This module defines basic functionality for indexed optics.  See the "Indexed
-- optics" section of the overview documentation in the @Optics@ module of the
-- main @optics@ package for more details.
--
module Optics.Indexed.Core
  (
  -- * Class for optic kinds that can be indexed
    IxOptic(..)

  , conjoined

  -- * Composition of indexed optics
  , (%)
  , (<%>)
  , (%>)
  , (<%)
  , reindexed
  , icompose
  , icompose3
  , icompose4
  , icompose5
  , icomposeN

    -- * Indexed optic flavours
  , module Optics.IxAffineFold
  , module Optics.IxAffineTraversal
  , module Optics.IxFold
  , module Optics.IxGetter
  , module Optics.IxLens
  , module Optics.IxSetter
  , module Optics.IxTraversal

  -- * Functors with index
  , FunctorWithIndex (..)
  -- ** Foldable with index
  , FoldableWithIndex (..)
  , itraverse_
  , ifor_
  , itoList
  -- ** Traversable with index
  , TraversableWithIndex (..)
  , ifor
  ) where

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Indexed.Classes
import Optics.Internal.Optic

import Optics.AffineFold
import Optics.AffineTraversal
import Optics.Fold
import Optics.Getter
import Optics.IxAffineFold
import Optics.IxAffineTraversal
import Optics.IxFold
import Optics.IxGetter
import Optics.IxLens
import Optics.IxSetter
import Optics.IxTraversal
import Optics.Lens
import Optics.Setter
import Optics.Traversal

-- | Compose two indexed optics. Their indices are composed as a pair.
--
-- >>> itoListOf (ifolded <%> ifolded) ["foo", "bar"]
-- [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
infixl 9 <%>
(<%>)
  :: (m ~ Join k l, Is k m, Is l m, IxOptic m s t a b,
      is `HasSingleIndex` i, js `HasSingleIndex` j)
  => Optic k is              s t u v
  -> Optic l js              u v a b
  -> Optic m (WithIx (i, j)) s t a b
o <%> o' = icompose (,) (o % o')
{-# INLINE (<%>) #-}

-- | Compose two indexed optics and drop indices of the left one. (If you want
-- to compose a non-indexed and an indexed optic, you can just use ('%').)
--
-- >>> itoListOf (ifolded %> ifolded) ["foo", "bar"]
-- [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]
--
infixl 9 %>
(%>)
  :: (m ~ Join k l, Is k m, Is l m, IxOptic k s t u v, NonEmptyIndices is)
  => Optic k is s t u v
  -> Optic l js u v a b
  -> Optic m js s t a b
o %> o' = noIx o % o'
{-# INLINE (%>) #-}

-- | Compose two indexed optics and drop indices of the right one. (If you want
-- to compose an indexed and a non-indexed optic, you can just use ('%').)
--
-- >>> itoListOf (ifolded <% ifolded) ["foo", "bar"]
-- [(0,'f'),(0,'o'),(0,'o'),(1,'b'),(1,'a'),(1,'r')]
--
infixl 9 <%
(<%)
  :: (m ~ Join k l, Is l m, Is k m, IxOptic l u v a b, NonEmptyIndices js)
  => Optic k is s t u v
  -> Optic l js u v a b
  -> Optic m is s t a b
o <% o' = o % noIx o'
{-# INLINE (<%) #-}

-- | Remap the index.
--
-- >>> itoListOf (reindexed succ ifolded) "foo"
-- [(1,'f'),(2,'o'),(3,'o')]
--
-- >>> itoListOf (ifolded %& reindexed succ) "foo"
-- [(1,'f'),(2,'o'),(3,'o')]
--
reindexed
  :: is `HasSingleIndex` i
  => (i -> j)
  -> Optic k is         s t a b
  -> Optic k (WithIx j) s t a b
reindexed = icomposeN
{-# INLINE reindexed #-}

-- | Flatten indices obtained from two indexed optics.
--
-- >>> itoListOf (ifolded % ifolded %& icompose (,)) ["foo","bar"]
-- [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
icompose
  :: (i -> j -> ix)
  -> Optic k '[i, j]     s t a b
  -> Optic k (WithIx ix) s t a b
icompose = icomposeN
{-# INLINE icompose #-}

-- | Flatten indices obtained from three indexed optics.
--
-- >>> itoListOf (ifolded % ifolded % ifolded %& icompose3 (,,)) [["foo","bar"],["xyz"]]
-- [((0,0,0),'f'),((0,0,1),'o'),((0,0,2),'o'),((0,1,0),'b'),((0,1,1),'a'),((0,1,2),'r'),((1,0,0),'x'),((1,0,1),'y'),((1,0,2),'z')]
--
icompose3
  :: (i1 -> i2 -> i3 -> ix)
  -> Optic k '[i1, i2, i3] s t a b
  -> Optic k (WithIx ix)   s t a b
icompose3 = icomposeN
{-# INLINE icompose3 #-}

-- | Flatten indices obtained from four indexed optics.
icompose4
  :: (i1 -> i2 -> i3 -> i4 -> ix)
  -> Optic k '[i1, i2, i3, i4] s t a b
  -> Optic k (WithIx ix)       s t a b
icompose4 = icomposeN
{-# INLINE icompose4 #-}

-- | Flatten indices obtained from five indexed optics.
icompose5
  :: (i1 -> i2 -> i3 -> i4 -> i5 -> ix)
  -> Optic k '[i1, i2, i3, i4, i5] s t a b
  -> Optic k (WithIx ix)           s t a b
icompose5 = icomposeN
{-# INLINE icompose5 #-}

-- | Flatten indices obtained from arbitrary number of indexed optics.
icomposeN
  :: forall k i is s t a b
  . (CurryCompose is, NonEmptyIndices is)
  => Curry is i
  -> Optic k is         s t a b
  -> Optic k (WithIx i) s t a b
icomposeN f (Optic o) = Optic (ixcontramap (\ij -> composeN @is ij f) . o)
{-# INLINE icomposeN #-}

----------------------------------------
-- IxOptic

-- | Class for optic kinds that can have indices.
class IxOptic k s t a b where
  -- | Convert an indexed optic to its unindexed equivalent.
  noIx
    :: NonEmptyIndices is
    => Optic k is   s t a b
    -> Optic k NoIx s t a b

instance (s ~ t, a ~ b) => IxOptic A_Getter s t a b where
  noIx o = to (view o)
  {-# INLINE noIx #-}

instance IxOptic A_Lens s t a b where
  noIx o = lensVL (toLensVL o)
  {-# INLINE noIx #-}

instance IxOptic An_AffineTraversal s t a b where
  noIx o = atraversalVL (atraverseOf o)
  {-# INLINE noIx #-}

instance (s ~ t, a ~ b) => IxOptic An_AffineFold s t a b where
  noIx o = afolding (preview o)
  {-# INLINE noIx #-}

instance IxOptic A_Traversal s t a b where
  noIx o = traversalVL (traverseOf o)
  {-# INLINE noIx #-}

instance (s ~ t, a ~ b) => IxOptic A_Fold s t a b where
  noIx o = foldVL (traverseOf_ o)
  {-# INLINE noIx #-}

instance IxOptic A_Setter s t a b where
  noIx o = sets (over o)
  {-# INLINE noIx #-}

-- $setup
-- >>> import Optics.Core
