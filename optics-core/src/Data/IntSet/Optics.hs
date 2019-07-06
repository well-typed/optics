-- |
-- Module: Data.IntSet.Optics
-- Description: Optics for working with 'IntSet's.
--
-- This module defines optics for constructing and manipulating finite 'IntSet's.
--
module Data.IntSet.Optics
  ( members
  , setmapped
  , setOf
  ) where

import Data.IntSet as IntSet

import Optics.Fold
import Optics.Setter

-- | IntSet isn't Foldable, but this 'Fold' can be used to access the members of
-- an 'IntSet'.
--
-- >>> sumOf members $ setOf folded [1,2,3,4]
-- 10
members :: Fold IntSet Int
members = folding IntSet.toAscList
{-# INLINE members #-}

-- | This 'Setter' can be used to change the type of a 'IntSet' by mapping the
-- elements to new values.
--
-- Sadly, you can't create a valid 'Optics.Traversal.Traversal' for an 'IntSet',
-- but you can manipulate it by reading using 'Optics.Fold.folded' and
-- reindexing it via 'setmapped'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: Setter' IntSet Int
setmapped = sets IntSet.map
{-# INLINE setmapped #-}

-- | Construct an 'IntSet' from a fold.
--
-- >>> setOf folded [1,2,3,4]
-- fromList [1,2,3,4]
--
-- >>> setOf (folded % _2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
setOf :: Is k A_Fold => Optic' k is s Int -> s -> IntSet
setOf l = foldMapOf l IntSet.singleton
{-# INLINE setOf #-}

-- $setup
-- >>> import Optics.Core
