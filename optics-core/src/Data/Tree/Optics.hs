-- |
-- Module: Data.Tree.Optics
-- Description: Optics for working with 'Tree's.
--
-- This module defines optics for manipulating 'Tree's.
--
module Data.Tree.Optics
  ( root
  , branches
  ) where

import Data.Tree

import Optics.Lens

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

-- $setup
-- >>> import Optics.Core
