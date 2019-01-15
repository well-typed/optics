module Optics.Fold
  ( A_Fold
  , Fold
  , toFold
  , ViewableOptic(..)
  , viewM
  , viewN
  , preview
  , foldMapOf
  , foldrOf
  , foldlOf'
  , toListOf
  , sequenceOf_
  , traverseOf_
  , folded
  , folding
    -- * Concrete folds
  , andOf
  , orOf
  , allOf
  , anyOf
  , noneOf
  , productOf
  , sumOf
  , asumOf
  , msumOf
  , elemOf
  , notElemOf
  , concatOf
  , concatMapOf
  , lengthOf
  , module Optics.Optic
  )
  where

import Control.Applicative
import Control.Monad
import Data.Monoid

import Optics.Internal.Fold
import Optics.Internal.Optic
import Optics.Internal.Utils
import Optics.Internal.View
import Optics.Optic

-- Concrete folds

andOf :: Is k A_Fold => Optic' k is s Bool -> s -> Bool
andOf o = getAll #. foldMapOf o All
{-# INLINE andOf #-}

orOf :: Is k A_Fold => Optic' k is s Bool -> s -> Bool
orOf o = getAny #. foldMapOf o Any
{-# INLINE orOf #-}

allOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
allOf o f = getAll #. foldMapOf o (All #. f)
{-# INLINE allOf #-}

anyOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
anyOf o f = getAny #. foldMapOf o (Any #. f)
{-# INLINE anyOf #-}

noneOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
noneOf o f = not . anyOf o f
{-# INLINE noneOf #-}

productOf :: (Is k A_Fold, Num a) => Optic' k is s a -> s -> a
productOf o = foldlOf' o (*) 1
{-# INLINE productOf #-}

sumOf :: (Is k A_Fold, Num a) => Optic' k is s a -> s -> a
sumOf o = foldlOf' o (+) 0
{-# INLINE sumOf #-}

asumOf :: (Is k A_Fold, Alternative f) => Optic' k is s (f a) -> s -> f a
asumOf o = foldrOf o (<|>) empty
{-# INLINE asumOf #-}

msumOf :: (Is k A_Fold, MonadPlus m) => Optic' k is s (m a) -> s -> m a
msumOf o = foldrOf o mplus mzero
{-# INLINE msumOf #-}

elemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
elemOf o = anyOf o . (==)
{-# INLINE elemOf #-}

notElemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
notElemOf o = allOf o . (/=)
{-# INLINE notElemOf #-}

concatOf :: Is k A_Fold => Optic' k is s [a] -> s -> [a]
concatOf o = foldMapOf o id
{-# INLINE concatOf #-}

concatMapOf :: Is k A_Fold => Optic' k is s a -> (a -> [b]) -> s -> [b]
concatMapOf = foldMapOf
{-# INLINE concatMapOf #-}

lengthOf :: Is k A_Fold => Optic' k is s a -> s -> Int
lengthOf o = foldlOf' o (\ n _ -> 1 + n) 0
{-# INLINE lengthOf #-}
