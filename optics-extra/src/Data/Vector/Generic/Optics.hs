-- | This module provides lenses and traversals for working with generic
-- vectors.
module Data.Vector.Generic.Optics
  ( toVectorOf
  -- * Isomorphisms
  , forced
  , vector
  , asStream
  , asStreamR
  , cloned
  , converted
  -- * Lenses
  , sliced
  -- * Traversal of individual indices
  , ordinals
  , vectorIx
  , vectorTraverse
  ) where

import Data.Vector.Fusion.Bundle (Bundle)
import Data.Vector.Generic as V hiding (zip, filter, indexed)
import Data.Vector.Generic.New (New)
import Prelude hiding ((++), length, null, head, tail, init, last, map, reverse)

import Optics.Core
import Optics.Extra.Internal.Vector
import Optics.Internal.Fold
import Optics.Internal.IxFold
import Optics.Internal.IxTraversal
import Optics.Internal.Profunctor
import Optics.Internal.Optic

-- $setup
-- >>> import Data.Vector as Vector

-- | @sliced i n@ provides a 'Lens' that edits the @n@ elements starting at
-- index @i@ from a 'Lens'.
--
-- This is only a valid 'Lens' if you do not change the length of the resulting
-- 'Vector'.
--
-- Attempting to return a longer or shorter vector will result in violations of
-- the 'Lens' laws.
--
-- >>> Vector.fromList [1..10] ^. sliced 2 5 == Vector.fromList [3,4,5,6,7]
-- True
--
-- >>> (Vector.fromList [1..10] & sliced 2 5 . mapped .~ 0) == Vector.fromList [1,2,0,0,0,0,0,8,9,10]
-- True
sliced
  :: Vector v a
  => Int -- ^ @i@ starting index
  -> Int -- ^ @n@ length
  -> Lens' (v a) (v a)
sliced i n = lensVL $ \f v ->
  (\v0 -> v // zip [i..i+n-1] (V.toList v0)) <$> f (slice i n v)
{-# INLINE sliced #-}

-- | Similar to 'toListOf', but returning a 'Vector'.
--
-- >>> (toVectorOf both (8,15) :: Vector.Vector Int) == Vector.fromList [8,15]
-- True
toVectorOf
  :: (Is k A_Fold, Vector v a)
  => Optic' k is s a
  -> s
  -> v a
toVectorOf l s = fromList (toListOf l s)
{-# INLINE toVectorOf #-}

-- | Convert a list to a 'Vector' (or back.)
--
-- >>> ([1,2,3] ^. vector :: Vector.Vector Int) == Vector.fromList [1,2,3]
-- True
--
-- >>> Vector.fromList [0,8,15] ^. from vector
-- [0,8,15]
vector
  :: (Vector v a, Vector v b)
  => Iso [a] [b] (v a) (v b)
vector = iso fromList V.toList
{-# INLINE vector #-}

-- | Convert a 'Vector' to a finite 'Bundle' (or back.)
asStream
  :: (Vector v a, Vector v b)
  => Iso (v a) (v b) (Bundle v a) (Bundle v b)
asStream = iso stream unstream
{-# INLINE asStream #-}

-- | Convert a 'Vector' to a finite 'Bundle' from right to left (or back.)
asStreamR
  :: (Vector v a, Vector v b)
  => Iso (v a) (v b) (Bundle v a) (Bundle v b)
asStreamR = iso streamR unstreamR
{-# INLINE asStreamR #-}

-- | Convert a 'Vector' back and forth to an initializer that when run produces
-- a copy of the 'Vector'.
cloned :: Vector v a => Iso' (v a) (New v a)
cloned = iso clone new
{-# INLINE cloned #-}

-- | Convert a 'Vector' to a version that doesn't retain any extra memory.
forced :: (Vector v a, Vector v b) => Iso (v a) (v b) (v a) (v b)
forced = iso force force
{-# INLINE forced #-}

-- | This 'Traversal' will ignore any duplicates in the supplied list of
-- indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
ordinals :: forall v a. Vector v a => [Int] -> IxTraversal' Int (v a) a
ordinals is = ixTraversalVL $ \f v ->
  (v //) <$> traverse (\i -> (,) i <$> f i (v ! i)) (ordinalNub (length v) is)
{-# INLINE ordinals #-}

-- | Like 'ix' but polymorphic in the vector type.
vectorIx :: V.Vector v a => Int -> Traversal' (v a) a
vectorIx i = traversalVL $ \f v ->
  if 0 <= i && i < V.length v
  then (\a -> v V.// [(i, a)]) <$> f (v V.! i)
  else pure v
{-# INLINE vectorIx #-}

-- | Indexed vector traversal for a generic vector.
vectorTraverse
  :: forall v w a b. (V.Vector v a, V.Vector w b)
  => IxTraversal Int (v a) (w b) a b
vectorTraverse = Optic vectorTraverse__
{-# INLINE vectorTraverse #-}

-- | Different vector implementations are isomorphic to each other.
converted
  :: (Vector v a, Vector w a, Vector v b, Vector w b)
  => Iso (v a) (v b) (w a) (w b)
converted = iso convert convert
{-# INLINE converted #-}

----------------------------------------
-- Internal implementations

vectorTraverse__
  :: forall p j v w a b. (Traversing p, V.Vector v a, V.Vector w b)
  => Optic__ p j (Int -> j) (v a) (w b) a b
vectorTraverse__ = conjoinedTraversal__ noix withix
  where
    noix :: Applicative f => (a -> f b) -> v a -> f (w b)
    noix f v = let !n = V.length v in V.fromListN n <$> traverse f (V.toList v)

    withix :: Applicative f => (Int -> a -> f b) -> (v a) -> f (w b)
    withix f v = let !n = V.length v in V.fromListN n <$> itraverse f (V.toList v)
{-# INLINE [0] vectorTraverse__ #-}

{-# RULES

"vectorTraverse__ -> mapped"
  forall (o :: FunArrow j a b). vectorTraverse__ o = roam V.map (reFunArrow o)
    :: (V.Vector v a, V.Vector v b) => FunArrow (Int -> j) (v a) (v b)

"vectorTraverse__ -> imapped"
  forall (o :: IxFunArrow j a b). vectorTraverse__ o = iroam V.imap o
    :: (V.Vector v a, V.Vector v b) => IxFunArrow (Int -> j) (v a) (v b)

"vectorTraverse__ -> foldr"
  forall (o :: Forget r j a b). vectorTraverse__ o = foldring__ V.foldr (reForget o)
    :: (V.Vector v a, V.Vector v b) => Forget r (Int -> j) (v a) (v b)

"vectorTraverse__ -> ifoldr"
  forall (o :: IxForget r j a b). vectorTraverse__ o = ifoldring__ V.ifoldr o
    :: (V.Vector v a, V.Vector v b) => IxForget r (Int -> j) (v a) (v b)

#-}
