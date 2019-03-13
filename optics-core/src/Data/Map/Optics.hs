{-# LANGUAGE CPP #-}
-- | One of most commonly-asked questions about this package is whether it
-- provides lenses for working with 'M.Map'. It does, but their uses are perhaps
-- obscured by their genericity. This module exists to provide documentation for
-- them.
--
-- 'M.Map' is an instance of 'Optics.At.Core.At' and provides
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
-- We can traverse, fold over, and map over key-value pairs in a 'M.Map',
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
module Data.Map.Optics
  ( toMapOf
  , at'
  ) where

import qualified Data.Map as M
import qualified Data.Map.Strict as Strict

import Optics.IxAffineTraversal
import Optics.IxFold
import Optics.Lens

-- $setup
-- >>> import Data.Monoid

-- | Construct a map from an 'IxFold'.
--
-- The construction is left-biased (see 'M.union'), i.e. the first
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
  => Optic' k is s a -> s -> M.Map i a
toMapOf o = ifoldMapOf o M.singleton
{-# INLINE toMapOf #-}

-- | Strict version of 'Optics.At.Core.at' for 'M.Map'.
at' :: Ord k => k -> Lens' (M.Map k a) (Maybe a)
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
