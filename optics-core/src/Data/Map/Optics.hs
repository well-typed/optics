{-# LANGUAGE CPP #-}
-- |
-- Module: Data.Map.Optics
-- Description: Optics for working with 'Data.Map.Map's.
--
-- This module exists to provide documentation for lenses for working with
-- 'Map', which might otherwise be obscured by their genericity.
--
-- 'Map' is an instance of 'Optics.At.Core.At' and provides 'Optics.At.Core.at'
-- as a lens on values at keys:
--
-- >>> Map.fromList [(1, "world")] ^. at 1
-- Just "world"
--
-- >>> Map.empty & at 1 .~ Just "world"
-- fromList [(1,"world")]
--
-- >>> Map.empty & at 0 .~ Just "hello"
-- fromList [(0,"hello")]
--
-- We can traverse, fold over, and map over key-value pairs in a 'Map',
-- thanks to indexed traversals, folds and setters.
--
-- >>> iover imapped const $ Map.fromList [(1, "Venus")]
-- fromList [(1,1)]
--
-- >>> ifoldMapOf ifolded (\i _ -> Sum i) $ Map.fromList [(2, "Earth"), (3, "Mars")]
-- Sum {getSum = 5}
--
-- >>> itraverseOf_ ifolded (curry print) $ Map.fromList [(4, "Jupiter")]
-- (4,"Jupiter")
--
-- >>> itoListOf ifolded $ Map.fromList [(5, "Saturn")]
-- [(5,"Saturn")]
--
-- A related class, 'Optics.At.Core.Ixed', allows us to use 'Optics.At.Core.ix' to
-- traverse a value at a particular key.
--
-- >>> Map.fromList [(2, "Earth")] & ix 2 %~ ("New " ++)
-- fromList [(2,"New Earth")]
--
-- >>> preview (ix 8) Map.empty
-- Nothing
--
module Data.Map.Optics
  ( toMapOf
  , lt
  , gt
  , le
  , ge
  ) where

import Data.Map as Map

import Optics.IxAffineTraversal
import Optics.IxFold

-- | Construct a map from an 'IxFold'.
--
-- The construction is left-biased (see 'Map.union'), i.e. the first
-- occurences of keys in the fold or traversal order are preferred.
--
-- >>> toMapOf ifolded ["hello", "world"]
-- fromList [(0,"hello"),(1,"world")]
--
-- >>> toMapOf (folded % ifolded) [('a',"alpha"),('b', "beta")]
-- fromList [('a',"alpha"),('b',"beta")]
--
-- >>> toMapOf (ifolded <%> ifolded) ["foo", "bar"]
-- fromList [((0,0),'f'),((0,1),'o'),((0,2),'o'),((1,0),'b'),((1,1),'a'),((1,2),'r')]
--
-- >>> toMapOf (folded % ifolded) [('a', "hello"), ('b', "world"), ('a', "dummy")]
-- fromList [('a',"hello"),('b',"world")]
--
toMapOf
  :: (Is k A_Fold, is `HasSingleIndex` i, Ord i)
  => Optic' k is s a -> s -> Map i a
toMapOf o = ifoldMapOf o Map.singleton
{-# INLINE toMapOf #-}

-- | Focus on the largest key smaller than the given one and its corresponding
-- value.
--
-- >>> Map.fromList [('a', "hi"), ('b', "there")] & over (lt 'b') (++ "!")
-- fromList [('a',"hi!"),('b',"there")]
--
-- >>> ipreview (lt 'a') $ Map.fromList [('a', 'x'), ('b', 'y')]
-- Nothing
lt :: Ord k => k -> IxAffineTraversal' k (Map k v) v
lt k = ixAtraversalVL $ \point f s ->
  case lookupLT k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> Map.insert k' v' s
{-# INLINE lt #-}

-- | Focus on the smallest key greater than the given one and its corresponding
-- value.
--
-- >>> Map.fromList [('a', "hi"), ('b', "there")] & over (gt 'b') (++ "!")
-- fromList [('a',"hi"),('b',"there")]
--
-- >>> ipreview (gt 'a') $ Map.fromList [('a', 'x'), ('b', 'y')]
-- Just ('b','y')
gt :: Ord k => k -> IxAffineTraversal' k (Map k v) v
gt k = ixAtraversalVL $ \point f s ->
  case lookupGT k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> Map.insert k' v' s
{-# INLINE gt #-}

-- | Focus on the largest key smaller or equal than the given one and its
-- corresponding value.
--
-- >>> Map.fromList [('a', "hi"), ('b', "there")] & over (le 'b') (++ "!")
-- fromList [('a',"hi"),('b',"there!")]
--
-- >>> ipreview (le 'a') $ Map.fromList [('a', 'x'), ('b', 'y')]
-- Just ('a','x')
le :: Ord k => k -> IxAffineTraversal' k (Map k v) v
le k = ixAtraversalVL $ \point f s ->
  case lookupLE k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> Map.insert k' v' s
{-# INLINE le #-}

-- | Focus on the smallest key greater or equal than the given one and its
-- corresponding value.
--
-- >>> Map.fromList [('a', "hi"), ('c', "there")] & over (ge 'b') (++ "!")
-- fromList [('a',"hi"),('c',"there!")]
--
-- >>> ipreview (ge 'b') $ Map.fromList [('a', 'x'), ('c', 'y')]
-- Just ('c','y')
ge :: Ord k => k -> IxAffineTraversal' k (Map k v) v
ge k = ixAtraversalVL $ \point f s ->
  case lookupGE k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> Map.insert k' v' s
{-# INLINE ge #-}

-- $setup
-- >>> import Data.Monoid
-- >>> import Optics.Core
