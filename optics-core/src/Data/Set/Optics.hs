-- |
-- Module: Data.Set.Optics
-- Description: Optics for working with 'Set's.
--
-- This module defines optics for constructing and manipulating finite 'Set's.
--
module Data.Set.Optics
  ( setmapped
  , setOf
  ) where

import Data.Set as Set

import Optics.Fold
import Optics.Optic
import Optics.Setter

-- | This 'Setter' can be used to change the type of a 'Set' by mapping the
-- elements to new values.
--
-- Sadly, you can't create a valid 'Optics.Traversal.Traversal' for a 'Set', but
-- you can manipulate it by reading using 'Optics.Fold.folded' and reindexing it
-- via 'setmapped'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: Ord b => Setter (Set a) (Set b) a b
setmapped = sets Set.map
{-# INLINE setmapped #-}

-- | Construct a set from a fold.
--
-- >>> setOf folded ["hello","world"]
-- fromList ["hello","world"]
--
-- >>> setOf (folded % _2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
setOf :: (Is k A_Fold, Ord a) => Optic' k is s a -> s -> Set a
setOf l = foldMapOf l Set.singleton
{-# INLINE setOf #-}

-- $setup
-- >>> import Optics.Core
