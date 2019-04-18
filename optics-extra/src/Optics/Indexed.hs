{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module: Optics.Indexed
-- Description: Definitions of indexed optics.
--
-- This module defines general functionality for indexed optics.  See the
-- "Indexed optics" section of the overview documentation in the @Optics@ module
-- of the main @optics@ package for more details.
--
-- Unlike "Optics.Indexed.Core", this includes the definitions from modules for
-- specific indexed optic flavours such as "Optics.IxTraversal", and includes
-- additional instances for 'FunctorWithIndex' and similar classes.
--
module Optics.Indexed
  (
    -- * Class for optic kinds that can be indexed
    IxOptic(..)

    -- * Composition of indexed optics
  , (<%>)
  , (%>)
  , (<%)
  , reindexed
  , icompose
  , icompose3
  , icompose4
  , icompose5

    -- * Indexed optic flavours
  , module Optics.IxAffineFold
  , module Optics.IxAffineTraversal
  , module Optics.IxFold
  , module Optics.IxSetter
  , module Optics.IxTraversal

  -- * Functors with index
  , FunctorWithIndex (..)
  -- ** Foldable with index
  , FoldableWithIndex (..)
  , itraverse_
  , ifor_
  -- ** Traversable with index
  , TraversableWithIndex (..)
  , ifor
  ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V

import Optics.Indexed.Core
import Optics.Internal.Indexed
import Optics.IxAffineFold
import Optics.IxAffineTraversal
import Optics.IxFold
import Optics.IxSetter
import Optics.IxTraversal

----------------------------------------
-- Extra *WithIndex instances

-- HashMap

instance FunctorWithIndex k (HM.HashMap k) where
  imap = HM.mapWithKey
  {-# INLINE imap #-}
instance FoldableWithIndex k (HM.HashMap k) where
  ifoldr  = HM.foldrWithKey
  ifoldl' = HM.foldlWithKey' . flip
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
instance TraversableWithIndex k (HM.HashMap k) where
  itraverse = HM.traverseWithKey
  {-# INLINE itraverse #-}

-- Vector

instance FunctorWithIndex Int V.Vector where
  imap = V.imap
  {-# INLINE imap #-}
instance FoldableWithIndex Int V.Vector where
  ifoldMap f = ifoldr (\i -> mappend . f i) mempty
  ifoldr     = V.ifoldr
  ifoldl'    = V.ifoldl' . flip
  {-# INLINE ifoldMap #-}
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
instance TraversableWithIndex Int V.Vector where
  itraverse f v =
    let !n = V.length v in V.fromListN n <$> itraverse f (V.toList v)
  {-# INLINE itraverse #-}
