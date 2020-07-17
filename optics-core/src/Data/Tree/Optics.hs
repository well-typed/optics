-- |
-- Module: Data.Tree.Optics
-- Description: Optics for working with 'Tree's.
--
-- This module defines optics for manipulating 'Tree's.
--
module Data.Tree.Optics
  ( root
  , branches
  , level
  , incrLevels
  ) where

import Data.Tree

import Optics.Lens
import Optics.Monoidal
import Optics.Optic
import Optics.Traversal

-- | A 'Lens' that focuses on the root of a 'Tree'.
--
-- >>> view root $ Node 42 []
-- 42
root :: Lens' (Tree a) a
root = lensVL $ \f (Node a as) -> (`Node` as) <$> f a
{-# INLINE root #-}

-- | A 'Lens' returning the direct descendants of the root of a 'Tree'
--
-- @'Optics.Getter.view' 'branches' ≡ 'subForest'@
branches :: Lens' (Tree a) [Tree a]
branches = lensVL $ \f (Node a as) -> Node a <$> f as
{-# INLINE branches #-}

-- | A 'Traversal' which returns the elements on a level.
level :: Int -> Traversal' (Tree a) a
level n
  | n <  0    = nothing
  | n == 0    = castOptic root
  | otherwise = branches % traversed % level (n-1)

-- | A 'Traversal' which returns the elements
--   on increasing order of level, up to sum cut-off.
incrLevels :: Int -> Traversal' (Tree a) a
incrLevels cut = go 0
  where go n | n > cut   = nothing
             | otherwise = level n <++> go (n+1)

-- $setup
-- >>> import Optics.Core
