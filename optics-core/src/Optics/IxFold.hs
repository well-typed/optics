{-# LANGUAGE DataKinds #-}
module Optics.IxFold
  ( A_Fold
  , IxFold
  , toIxFold
  , ixFoldVL
  , conjoinedFold
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf'
  , itoListOf
  , itraverseOf_
  , iforOf_
  , ifolded
  , ifoldring
  , module Optics.Optic
  ) where

import Data.Monoid

import Optics.Internal.Indexed
import Optics.Internal.IxFold
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils
import Optics.Optic

-- | Type synonym for an indexed fold.
type IxFold i s a = Optic' A_Fold (WithIx i) s a

-- | Explicitly cast an optic to an indexed fold.
toIxFold :: Is k A_Fold => Optic' k (WithIx i) s a -> IxFold i s a
toIxFold = castOptic
{-# INLINE toIxFold #-}

-- | Build an indexed fold from the "almost van Laarhoven" representation.
--
-- @
-- 'ixFoldVL' '.' 'itraverseOf_' ≡ 'id'
-- 'itraverseOf_' '.' 'ixFoldVL' ≡ 'id'
-- @
ixFoldVL
  :: (forall f. Applicative f => (i -> a -> f r) -> s -> f ())
  -> IxFold i s a
ixFoldVL f = Optic (ixFoldVL__ f)
{-# INLINE ixFoldVL #-}

-- | Build an indexed fold from the van Laarhoven representation of both its
-- unindexed and indexed version.
--
-- Appropriate version of the fold will be automatically picked for maximum
-- efficiency depending on whether it is used as indexed or regular one.
--
-- @
-- 'traverseOf_'  ('conjoinedFold' f g) ≡ 'traverseOf_'  ('foldVL' f)
-- 'itraverseOf_' ('conjoinedFold' f g) ≡ 'itraverseOf_' ('ixFoldVL' g)
-- @
conjoinedFold
  :: (forall f. Applicative f => (     a -> f r) -> s -> f ())
  -> (forall f. Applicative f => (i -> a -> f r) -> s -> f ())
  -> IxFold i s a
conjoinedFold f g = Optic (conjoinedFold__ f g)
{-# INLINE conjoinedFold #-}

-- | Fold with index via embedding into a monoid.
ifoldMapOf
  :: (Is k A_Fold, Monoid m, (is `HasSingleIndex` i) "ifoldMapOf" 1)
  => Optic' k is s a
  -> (i -> a -> m) -> s -> m
ifoldMapOf o f = runIxForget (getOptic (toIxFold o) (IxForget f)) id
{-# INLINE ifoldMapOf #-}

-- | Fold with index right-associatively.
ifoldrOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ifoldrOf" 1)
  => Optic' k is s a
  -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf o iarr r0 = (\e -> appEndo e r0) . ifoldMapOf o (\i -> Endo #. iarr i)
{-# INLINE ifoldrOf #-}

-- | Fold with index left-associatively, and strictly.
ifoldlOf'
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ifoldlOf'" 1)
  => Optic' k is s a
  -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' o irar r0 s = ifoldrOf o (\i a rr r -> rr $! irar i r a) id s r0
{-# INLINE ifoldlOf' #-}

-- | Fold with index to a list.
--
-- >>> itoListOf (folded % ifolded) ["abc", "def"]
-- [(0,'a'),(1,'b'),(2,'c'),(0,'d'),(1,'e'),(2,'f')]
--
-- /Note:/ currently indexed optics can be used as non-indexed
--
-- >>> toListOf (folded % ifolded) ["abc", "def"]
-- "abcdef"
--
itoListOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "itoListOf" 1)
  => Optic' k is s a
  -> s -> [(i, a)]
itoListOf o = ifoldrOf o (\i -> (:) . (i, )) []
{-# INLINE itoListOf #-}

----------------------------------------

itraverseOf_
  :: (Is k A_Fold, Applicative f, (is `HasSingleIndex` i) "itraverseOf_" 1)
  => Optic' k is s a
  -> (i -> a -> f r) -> s -> f ()
itraverseOf_ o f s =
  -- We want to have the definition fully eta-expanded as it allows GHC to
  -- generate better code (in particular for folding over a Vector).
  runTraversed $ ifoldMapOf o (\i -> Traversed #. f i) s
{-# INLINE itraverseOf_ #-}

-- | A version of 'itraverseOf_' with the arguments flipped.
iforOf_
  :: (Is k A_Fold, Applicative f, (is `HasSingleIndex` i) "iforOf_" 1)
  => Optic' k is s a
  -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- | Indexed fold via 'FoldableWithIndex' class.
ifolded :: FoldableWithIndex i f => IxFold i (f a) a
ifolded = Optic ifolded__
{-# INLINE ifolded #-}

-- | Obtain an 'IxFold' by lifting 'ifoldr' like function.
--
-- >>> itoListOf (ifoldring ifoldr) "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
ifoldring
  :: (forall f. Applicative f => (i -> a -> f r -> f r) -> f r -> s -> f r)
  -> IxFold i s a
ifoldring fr = Optic (ifoldring__ fr)
{-# INLINE ifoldring #-}
