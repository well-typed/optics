-- | This module provides lenses and traversals for working with vectors.
module Data.Vector.Optics
  ( toVectorOf
  -- * Isomorphisms
  , vector
  , forced
  -- * Lenses
  , sliced
  -- * Traversal of individual indices
  , ordinals
  ) where

import Data.Vector (Vector)
import Optics.Core
import qualified Data.Vector.Generic.Optics as G

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
-- >>> (Vector.fromList [1..10] & sliced 2 5 % mapped .~ 0) == Vector.fromList [1,2,0,0,0,0,0,8,9,10]
-- True
sliced
  :: Int -- ^ @i@ starting index
  -> Int -- ^ @n@ length
  -> Lens' (Vector a) (Vector a)
sliced = G.sliced
{-# INLINE sliced #-}

-- | Similar to 'toListOf', but returning a 'Vector'.
--
-- >>> toVectorOf each (8,15) == Vector.fromList [8,15]
-- True
toVectorOf
  :: Is k A_Fold
  => Optic' k is s a
  -> s
  -> Vector a
toVectorOf = G.toVectorOf
{-# INLINE toVectorOf #-}

-- | Convert a list to a 'Vector' (or back)
--
-- >>> [1,2,3] ^. vector == Vector.fromList [1,2,3]
-- True
--
-- >>> [1,2,3] ^. vector % re vector
-- [1,2,3]
--
-- >>> Vector.fromList [0,8,15] ^. re vector % vector == Vector.fromList [0,8,15]
-- True
vector :: Iso [a] [b] (Vector a) (Vector b)
vector = G.vector
{-# INLINE vector #-}

-- | Convert a 'Vector' to a version that doesn't retain any extra
-- memory.
forced :: Iso (Vector a) (Vector b) (Vector a) (Vector b)
forced = G.forced
{-# INLINE forced #-}

-- | This 'Traversal' will ignore any duplicates in the supplied list of
-- indices.
--
-- >>> toListOf (ordinals [1,3,2,5,9,10]) $ Vector.fromList [2,4..40]
-- [4,8,6,12,20,22]
ordinals :: forall a. [Int] -> IxTraversal' Int (Vector a) a
ordinals = G.ordinals
{-# INLINE ordinals #-}
