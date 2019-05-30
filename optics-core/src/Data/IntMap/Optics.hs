{-# LANGUAGE CPP #-}
-- | 'M.IntMap' is an instance of 'Optics.At.Core.At' and provides
-- 'Optics.At.Core.at' as a lens on values at keys:
--
-- >>> M.fromList [(1, "world")] ^. at 1
-- Just "world"
--
-- >>> M.empty & at 1 .~ Just "world"
-- fromList [(1,"world")]
--
-- >>> M.empty & at 0 .~ Just "hello"
-- fromList [(0,"hello")]
--
-- We can traverse, fold over, and map over key-value pairs in a 'M.IntMap',
-- thanks to indexed traversals, folds and setters.
--
-- >>> iover imapped const $ M.fromList [(1, "Venus")]
-- fromList [(1,1)]
--
-- >>> ifoldMapOf ifolded (\i _ -> Sum i) $ M.fromList [(2, "Earth"), (3, "Mars")]
-- Sum {getSum = 5}
--
-- >>> itraverseOf_ ifolded (curry print) $ M.fromList [(4, "Jupiter")]
-- (4,"Jupiter")
--
-- >>> itoListOf ifolded $ M.fromList [(5, "Saturn")]
-- [(5,"Saturn")]
--
-- A related class, 'Optics.At.Core.Ixed', allows us to use 'Optics.At.Core.ix' to
-- traverse a value at a particular key.
--
-- >>> M.fromList [(2, "Earth")] & ix 2 %~ ("New " ++)
-- fromList [(2,"New Earth")]
--
-- >>> preview (ix 8) M.empty
-- Nothing
--
module Data.IntMap.Optics
  ( toMapOf
  , at'
  , lt
  , gt
  , le
  , ge
  ) where

import qualified Data.IntMap as M
import qualified Data.IntMap.Strict as Strict

import Optics.IxAffineTraversal
import Optics.IxFold
import Optics.Lens

-- $setup
-- >>> import Data.Monoid

-- | Construct a map from an 'IxFold'.
--
-- The construction is left-biased (see 'M.union'), i.e. the first occurences of
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
  => Optic' k is s a -> s -> M.IntMap a
toMapOf o = ifoldMapOf o M.singleton
{-# INLINE toMapOf #-}

-- | Strict version of 'Optics.At.Core.at' for 'M.IntMap'.
at' :: Int -> Lens' (M.IntMap a) (Maybe a)
at' k = lensVL $ \f s ->
#if MIN_VERSION_containers(0,5,8)
  Strict.alterF f k s
#else
  let mv = Strict.lookup k s
  in f mv <&> \case
    Nothing -> maybe s (\_ -> Strict.delete k s) mv
    Just v  -> Strict.insert k v s
#endif
{-# INLINE at' #-}

-- | Focus on the largest key smaller than the given one and its corresponding
-- value.
--
-- >>> M.fromList [(1, "hi"), (2, "there")] & over (lt 2) (++ "!")
-- fromList [(1,"hi!"),(2,"there")]
--
-- >>> ipreview (lt 1) $ M.fromList [(1, 'x'), (2, 'y')]
-- Nothing
lt :: Int -> IxAffineTraversal' Int (M.IntMap v) v
lt k = ixAtraversalVL $ \point f s ->
  case M.lookupLT k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> M.insert k' v' s
{-# INLINE lt #-}

-- | Focus on the smallest key greater than the given one and its corresponding
-- value.
--
-- >>> M.fromList [(1, "hi"), (2, "there")] & over (gt 2) (++ "!")
-- fromList [(1,"hi"),(2,"there")]
--
-- >>> ipreview (gt 1) $ M.fromList [(1, 'x'), (2, 'y')]
-- Just (2,'y')
gt :: Int -> IxAffineTraversal' Int (M.IntMap v) v
gt k = ixAtraversalVL $ \point f s ->
  case M.lookupGT k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> M.insert k' v' s
{-# INLINE gt #-}

-- | Focus on the largest key smaller or equal than the given one and its
-- corresponding value.
--
-- >>> M.fromList [(1, "hi"), (2, "there")] & over (le 2) (++ "!")
-- fromList [(1,"hi"),(2,"there!")]
--
-- >>> ipreview (le 1) $ M.fromList [(1, 'x'), (2, 'y')]
-- Just (1,'x')
le :: Int -> IxAffineTraversal' Int (M.IntMap v) v
le k = ixAtraversalVL $ \point f s ->
  case M.lookupLE k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> M.insert k' v' s
{-# INLINE le #-}

-- | Focus on the smallest key greater or equal than the given one and its
-- corresponding value.
--
-- >>> M.fromList [(1, "hi"), (3, "there")] & over (ge 2) (++ "!")
-- fromList [(1,"hi"),(3,"there!")]
--
-- >>> ipreview (ge 2) $ M.fromList [(1, 'x'), (3, 'y')]
-- Just (3,'y')
ge :: Int -> IxAffineTraversal' Int (M.IntMap v) v
ge k = ixAtraversalVL $ \point f s ->
  case M.lookupGE k s of
    Nothing      -> point s
    Just (k', v) -> f k' v <&> \v' -> M.insert k' v' s
{-# INLINE ge #-}
