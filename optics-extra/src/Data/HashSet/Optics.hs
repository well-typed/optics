-- |
-- Module: Data.HashSet.Optics
-- Description: Optics for working with 'HashSet's.
--
-- This module defines optics for constructing and manipulating finite
-- 'HashSet's.
--
module Data.HashSet.Optics
  ( setmapped
  , setOf
  ) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Optics.Fold
import Optics.Optic
import Optics.Setter

-- | This 'Setter' can be used to change the type of a 'HashSet' by mapping the
-- elements to new values.
--
-- Sadly, you can't create a valid 'Optics.Traversal.Traversal' for a 'HashSet',
-- but you can manipulate it by reading using 'Optics.Fold.folded' and
-- reindexing it via 'setmapped'.
--
-- >>> over setmapped (+1) (HashSet.fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: (Eq b, Hashable b) => Setter (HashSet a) (HashSet b) a b
setmapped = sets HashSet.map
{-# INLINE setmapped #-}

-- | Construct a 'HashSet' from a fold.
--
-- >>> setOf folded ["hello","world"]
-- fromList ["hello","world"]
--
-- >>> setOf (folded % _2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
setOf :: (Is k A_Fold, Eq a, Hashable a) => Optic' k is s a -> s -> HashSet a
setOf l = foldMapOf l HashSet.singleton
{-# INLINE setOf #-}

-- $setup
-- >>> import Optics.Core
