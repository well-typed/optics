{-# LANGUAGE CPP #-}
-- | 'IntMap' is an instance of 'Optics.At.Core.At' and provides
-- 'Optics.At.Core.at' as a lens on values at keys:
--
-- >>> IntMap.fromList [(1, "world")] ^. at 1
-- Just "world"
--
-- >>> IntMap.empty & at 1 .~ Just "world"
-- fromList [(1,"world")]
--
-- >>> IntMap.empty & at 0 .~ Just "hello"
-- fromList [(0,"hello")]
--
-- We can traverse, fold over, and map over key-value pairs in an 'IntMap',
-- thanks to indexed traversals, folds and setters.
--
-- >>> iover imapped const $ IntMap.fromList [(1, "Venus")]
-- fromList [(1,1)]
--
-- >>> ifoldMapOf ifolded (\i _ -> Sum i) $ IntMap.fromList [(2, "Earth"), (3, "Mars")]
-- Sum {getSum = 5}
--
-- >>> itraverseOf_ ifolded (curry print) $ IntMap.fromList [(4, "Jupiter")]
-- (4,"Jupiter")
--
-- >>> itoListOf ifolded $ IntMap.fromList [(5, "Saturn")]
-- [(5,"Saturn")]
--
-- A related class, 'Optics.At.Core.Ixed', allows us to use 'Optics.At.Core.ix' to
-- traverse a value at a particular key.
--
-- >>> IntMap.fromList [(2, "Earth")] & ix 2 %~ ("New " ++)
-- fromList [(2,"New Earth")]
--
-- >>> preview (ix 8) IntMap.empty
-- Nothing
--
module Data.IntMap.Optics
  ( toMapOf
  , lt
  , gt
  , le
  , ge
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Optics.IxAffineTraversal
import Optics.IxFold
import Optics.Optic

-- | Construct a map from an 'IxFold'.
--
-- The construction is left-biased (see 'IntMap.union'), i.e. the first occurrences of
-- keys in the fold or traversal order are preferred.
--
-- >>> toMapOf ifolded ["hello", "world"]
-- fromList [(0,"hello"),(1,"world")]
--
-- >>> toMapOf (folded % ifolded) [(1,"alpha"),(2, "beta")]
-- fromList [(1,"alpha"),(2,"beta")]
--
-- >>> toMapOf (icompose (\a b -> 10*a+b) $ ifolded % ifolded) ["foo", "bar"]
-- fromList [(0,'f'),(1,'o'),(2,'o'),(10,'b'),(11,'a'),(12,'r')]
--
-- >>> toMapOf (folded % ifolded) [(1, "hello"), (2, "world"), (1, "dummy")]
-- fromList [(1,"hello"),(2,"world")]
--
toMapOf
  :: (Is k A_Fold, is `HasSingleIndex` Int)
  => Optic' k is s a -> s -> IntMap a
toMapOf o = ifoldMapOf o IntMap.singleton
{-# INLINE toMapOf #-}

-- | Focus on the largest key smaller than the given one and its corresponding
-- value.
--
-- >>> IntMap.fromList [(1, "hi"), (2, "there")] & over (lt 2) (++ "!")
-- fromList [(1,"hi!"),(2,"there")]
--
-- >>> ipreview (lt 1) $ IntMap.fromList [(1, 'x'), (2, 'y')]
-- Nothing
lt :: Int -> IxAffineTraversal' Int (IntMap v) v
lt k = iatraversalVL $ \point f s ->
  case IntMap.lookupLT k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> IntMap.insert k' v' s
{-# INLINE lt #-}

-- | Focus on the smallest key greater than the given one and its corresponding
-- value.
--
-- >>> IntMap.fromList [(1, "hi"), (2, "there")] & over (gt 2) (++ "!")
-- fromList [(1,"hi"),(2,"there")]
--
-- >>> ipreview (gt 1) $ IntMap.fromList [(1, 'x'), (2, 'y')]
-- Just (2,'y')
gt :: Int -> IxAffineTraversal' Int (IntMap v) v
gt k = iatraversalVL $ \point f s ->
  case IntMap.lookupGT k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> IntMap.insert k' v' s
{-# INLINE gt #-}

-- | Focus on the largest key smaller or equal than the given one and its
-- corresponding value.
--
-- >>> IntMap.fromList [(1, "hi"), (2, "there")] & over (le 2) (++ "!")
-- fromList [(1,"hi"),(2,"there!")]
--
-- >>> ipreview (le 1) $ IntMap.fromList [(1, 'x'), (2, 'y')]
-- Just (1,'x')
le :: Int -> IxAffineTraversal' Int (IntMap v) v
le k = iatraversalVL $ \point f s ->
  case IntMap.lookupLE k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> IntMap.insert k' v' s
{-# INLINE le #-}

-- | Focus on the smallest key greater or equal than the given one and its
-- corresponding value.
--
-- >>> IntMap.fromList [(1, "hi"), (3, "there")] & over (ge 2) (++ "!")
-- fromList [(1,"hi"),(3,"there!")]
--
-- >>> ipreview (ge 2) $ IntMap.fromList [(1, 'x'), (3, 'y')]
-- Just (3,'y')
ge :: Int -> IxAffineTraversal' Int (IntMap v) v
ge k = iatraversalVL $ \point f s ->
  case IntMap.lookupGE k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> IntMap.insert k' v' s
{-# INLINE ge #-}

-- $setup
-- >>> import Data.Monoid
-- >>> import Optics.Core
