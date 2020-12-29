{-# LANGUAGE CPP #-}
-- |
-- Module: Data.HashMap.Optics
-- Description: Optics for working with 'Data.Map.HashMap's.
--
-- This module exists to provide documentation for lenses for working with
-- 'HashMap', which might otherwise be obscured by their genericity.
--
-- 'HashMap' is an instance of 'Optics.At.Core.At' and provides
-- 'Optics.At.Core.at' as a lens on values at keys:
--
-- >>> HashMap.fromList [(1, "world")] ^. at 1
-- Just "world"
--
-- >>> HashMap.empty & at 1 .~ Just "world"
-- fromList [(1,"world")]
--
-- >>> HashMap.empty & at 0 .~ Just "hello"
-- fromList [(0,"hello")]
--
-- We can traverse, fold over, and map over key-value pairs in a 'HashMap',
-- thanks to indexed traversals, folds and setters.
--
-- >>> iover imapped const $ HashMap.fromList [(1, "Venus")]
-- fromList [(1,1)]
--
-- >>> ifoldMapOf ifolded (\i _ -> Sum i) $ HashMap.fromList [(2, "Earth"), (3, "Mars")]
-- Sum {getSum = 5}
--
-- >>> itraverseOf_ ifolded (curry print) $ HashMap.fromList [(4, "Jupiter")]
-- (4,"Jupiter")
--
-- >>> itoListOf ifolded $ HashMap.fromList [(5, "Saturn")]
-- [(5,"Saturn")]
--
-- A related class, 'Optics.At.Core.Ixed', allows us to use 'Optics.At.Core.ix'
-- to traverse a value at a particular key.
--
-- >>> HashMap.fromList [(2, "Earth")] & ix 2 %~ ("New " ++)
-- fromList [(2,"New Earth")]
--
-- >>> preview (ix 8) HashMap.empty
-- Nothing
--
module Data.HashMap.Optics
  ( toMapOf
  , at'
  ) where

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

import Optics.Core

-- | Construct a hash map from an 'IxFold'.
--
-- The construction is left-biased (see 'HashMap.union'), i.e. the first
-- occurrences of keys in the fold or traversal order are preferred.
--
-- >>> toMapOf ifolded ["hello", "world"]
-- fromList [(0,"hello"),(1,"world")]
--
-- >>> toMapOf (folded % ifolded) [('a',"alpha"),('b', "beta")]
-- fromList [('a',"alpha"),('b',"beta")]
--
-- >>> toMapOf (folded % ifolded) [('a', "hello"), ('b', "world"), ('a', "dummy")]
-- fromList [('a',"hello"),('b',"world")]
--
toMapOf
  :: (Is k A_Fold, is `HasSingleIndex` i, Eq i, Hashable i)
  => Optic' k is s a -> s -> HashMap i a
toMapOf o = ifoldMapOf o HashMap.singleton
{-# INLINE toMapOf #-}

-- $setup
-- >>> import Data.Monoid
-- >>> import Optics.At ()
-- >>> import Optics.Indexed ()
