{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Optics.Indexed
  ( -- * Indexed optics
    module Optics.IxTraversal
  , module Optics.IxFold
  , module Optics.IxSetter

  -- * Index composition and modification
  , (<%>)
  , (%>)
  , (<%)
  , reindexed
  , icompose
  , icompose3
  , icompose4
  , icompose5
  , IxOptic(..)

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
