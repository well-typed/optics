-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set.Optics
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Set.Optics
  ( setmapped
  , setOf
  ) where

import Data.Set as Set

import Optics.Fold
import Optics.Setter

-- | This 'Setter' can be used to change the type of a 'Set' by mapping the
-- elements to new values.
--
-- Sadly, you can't create a valid 'Traversal' for a 'Set', but you can
-- manipulate it by reading using 'Optics.folded' and reindexing it via
-- 'setmapped'.
--
-- >>> over setmapped (+1) (fromList [1,2,3,4])
-- fromList [2,3,4,5]
setmapped :: Ord b => Setter i (Set a) (Set b) a b
setmapped = sets Set.map
{-# INLINE setmapped #-}

-- | Construct a set from a fold.
--
-- >>> setOf folded ["hello","world"]
-- fromList ["hello","world"]
--
-- >>> setOf (folded % _2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
setOf :: (Is k A_Fold, Ord a) => Optic' k i i s a -> s -> Set a
setOf l = foldMapOf l Set.singleton
{-# INLINE setOf #-}

-- $setup
-- >>> import Optics
-- >>> import Optics.Operators