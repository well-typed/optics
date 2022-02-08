module Control.Parallel.Strategies.Optics
  ( parOver
  , evalOf
  , parOf
  ) where

import qualified Control.Parallel.Strategies as Parallel
import Optics.Core

-- | Apply a traversal as a modifier in parallel.
parOver :: Is k A_Traversal => Parallel.Strategy b -> Optic k is s t a b -> (a -> b) -> s -> t
parOver strat o f = Parallel.runEval . traverseOf o (Parallel.rparWith strat . f)
{-# INLINE parOver #-}

-- | Evaluate the targets of a 'Lens' or 'Traversal' into a data structure
-- according to the given 'Strategy'.
--
-- @
-- 'evalTraversable' = 'evalOf' 'traverse' = 'traverse'
-- 'evalOf' = 'id'
-- @
--
-- @
-- 'evalOf' :: 'Lens'' s a -> 'Strategy' a -> 'Strategy' s
-- 'evalOf' :: 'Traversal'' s a -> 'Strategy' a -> 'Strategy' s
-- @
evalOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
evalOf = traverseOf
{-# INLINE evalOf #-}

-- | Evaluate the targets of a 'Lens' or 'Traversal' according into a
-- data structure according to a given 'Strategy' in parallel.
--
-- @'parTraversable' = 'parOf' 'traverse'@
--
-- @
-- 'parOf' :: 'Lens'' s a -> 'Strategy' a -> 'Strategy' s
-- 'parOf' :: 'Traversal'' s a -> 'Strategy' a -> 'Strategy' s
-- @
parOf :: Is k A_Traversal => Optic' k is s a -> Parallel.Strategy a -> Parallel.Strategy s
parOf o strat = traverseOf o $ Parallel.rparWith strat
{-# INLINE parOf #-}
