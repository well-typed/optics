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

  -- * Composition of indexed optics
  , (<%>)
  , (%>)
  , (<%)
  , reindexed
  , icompose
  , icompose3
  , icompose4
  , icompose5

  -- * Constraints
  , HasSingleIndex
  , NonEmptyIndices
  ) where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor

import Optics.AffineFold
import Optics.AffineTraversal
import Optics.Fold
import Optics.Setter
import Optics.Traversal

-- | Compose two indexed optics. Their indices are composed as a pair.
--
-- >>> itoListOf (ifolded <%> ifolded) ["foo", "bar"]
-- [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
infixr 9 <%>
(<%>)
  :: (m ~ Join k l, Is k m, Is l m, IxOptic m s t a b,
      is `HasSingleIndex` i, js `HasSingleIndex` j)
  => Optic k is              s t u v
  -> Optic l js              u v a b
  -> Optic m (WithIx (i, j)) s t a b
o <%> o' = icompose (,) (o % o')
{-# INLINE (<%>) #-}

-- | Compose two optics and preserve indices of the right one.
--
-- >>> itoListOf (ifolded %> ifolded) ["foo", "bar"]
-- [(0,'f'),(1,'o'),(2,'o'),(0,'b'),(1,'a'),(2,'r')]
--
infixr 9 %>
(%>)
  :: (m ~ Join k l, Is k m, Is l m, IxOptic k s t u v, NonEmptyIndices is)
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
  :: (m ~ Join k l, Is l m, Is k m, IxOptic l u v a b, NonEmptyIndices js)
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
reindexed
  :: (IxOptic k s t a b, is `HasSingleIndex` i)
  => (i -> j)
  -> Optic k is         s t a b
  -> Optic k (WithIx j) s t a b
reindexed = icomposeN
{-# INLINE reindexed #-}

-- | Flatten indices obtained from two indexed optics.
icompose
  :: IxOptic k s t a b
  => (i -> j -> ix)
  -> Optic k '[i, j]     s t a b
  -> Optic k (WithIx ix) s t a b
icompose = icomposeN
{-# INLINE icompose #-}

-- | Flatten indices obtained from three indexed optics.
icompose3
  :: IxOptic k s t a b
  => (i1 -> i2 -> i3 -> ix)
  -> Optic k '[i1, i2, i3] s t a b
  -> Optic k (WithIx ix)   s t a b
icompose3 = icomposeN
{-# INLINE icompose3 #-}

-- | Flatten indices obtained from four indexed optics.
icompose4
  :: IxOptic k s t a b
  => (i1 -> i2 -> i3 -> i4 -> ix)
  -> Optic k '[i1, i2, i3, i4] s t a b
  -> Optic k (WithIx ix)       s t a b
icompose4 = icomposeN
{-# INLINE icompose4 #-}

-- | Flatten indices obtained from five indexed optics.
icompose5
  :: IxOptic k s t a b
  => (i1 -> i2 -> i3 -> i4 -> i5 -> ix)
  -> Optic k '[i1, i2, i3, i4, i5] s t a b
  -> Optic k (WithIx ix)           s t a b
icompose5 = icomposeN
{-# INLINE icompose5 #-}

----------------------------------------
-- IxOptic

-- | Class for optic kinds that can have indices.
class IxOptic k s t a b where
  -- | Convert an indexed optic to its unindexed equivalent.
  noIx
    :: NonEmptyIndices is
    => Optic k is s t a b
    -> Optic k NoIx s t a b

  -- | Flatten indices obtained from arbitrary number of indexed optics.
  icomposeN
    :: (CurryCompose is, NonEmptyIndices is)
    => Curry is i
    -> Optic k is         s t a b
    -> Optic k (WithIx i) s t a b

  -- | Construct a conjoined indexed optic that provides a separate code path
  -- when used without indices. Useful for defining indexed optics that are as
  -- efficient as their unindexed equivalents when used without indices.
  --
  -- /Note:/ @'conjoined' f g@ is well-defined if and only if @f ≡ 'noIx' g@.
  conjoined
    :: is `HasSingleIndex` i
    => Optic k NoIx s t a b
    -> Optic k is   s t a b
    -> Optic k is   s t a b

instance IxOptic An_AffineTraversal s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = atraversalVL (toAtraversalVL o)
  {-# INLINE noIx #-}
  icomposeN f o = Optic (icomposeN__ f o)
  {-# INLINE icomposeN #-}
  conjoined f g = Optic (conjoined__ f g)
  {-# INLINE conjoined #-}

instance (s ~ t, a ~ b) => IxOptic An_AffineFold s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = afolding (preview o)
  {-# INLINE noIx #-}
  icomposeN f o = Optic (icomposeN__ f o)
  {-# INLINE icomposeN #-}
  conjoined f g = Optic (conjoined__ f g)
  {-# INLINE conjoined #-}

instance IxOptic A_Traversal s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = traversalVL (traverseOf o)
  {-# INLINE noIx #-}
  icomposeN f o = Optic (icomposeN__ f o)
  {-# INLINE icomposeN #-}
  conjoined f g = Optic (conjoined__ f g)
  {-# INLINE conjoined #-}

instance (s ~ t, a ~ b) => IxOptic A_Fold s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = mkFold (traverseOf_ o)
  {-# INLINE noIx #-}
  icomposeN f o = Optic (icomposeN__ f o)
  {-# INLINE icomposeN #-}
  conjoined f g = Optic (conjoined__ f g)
  {-# INLINE conjoined #-}

instance IxOptic A_Setter s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = sets (over o)
  {-# INLINE noIx #-}
  icomposeN f o = Optic (icomposeN__ f o)
  {-# INLINE icomposeN #-}
  conjoined f g = Optic (conjoined__ f g)
  {-# INLINE conjoined #-}

----------------------------------------
-- Internal

-- | Implementation of 'icomposeN'.
icomposeN__
  :: forall k p is i j s t a b
  . (Constraints k p, Visiting p, CurryCompose is)
  => Curry is i
  -> Optic k is s t a b
  -> Optic__ p j (i -> j) s t a b
icomposeN__ f o =
  ixcontramap (\ij -> composeN @is ij f) . getOptic o
{-# INLINE icomposeN__ #-}

-- $setup
-- >>> import Optics.Core
