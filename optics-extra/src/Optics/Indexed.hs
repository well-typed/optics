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
  , reindex
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

import Control.Applicative.Backwards
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Functor.Reverse
import Data.Monoid hiding (Product, Sum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V

import Optics.Indexed.Core
import Optics.Internal.Indexed
import Optics.Internal.Utils
import Optics.IxFold
import Optics.IxSetter
import Optics.IxTraversal

----------------------------------------
-- Extra *WithIndex instances

-- Backwards

instance FunctorWithIndex i f => FunctorWithIndex i (Backwards f) where
  imap f  = Backwards . imap f . forwards
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Backwards f) where
  ifoldMap f = ifoldMap f . forwards
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Backwards f) where
  itraverse f = fmap Backwards . itraverse f . forwards
  {-# INLINE itraverse #-}

-- Reverse

instance FunctorWithIndex i f => FunctorWithIndex i (Reverse f) where
  imap f = Reverse . imap f . getReverse
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Reverse f) where
  ifoldMap f = getDual . ifoldMap (\i -> Dual #. f i) . getReverse
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Reverse f) where
  itraverse f =
    fmap Reverse . forwards . itraverse (\i -> Backwards . f i) . getReverse
  {-# INLINE itraverse #-}

-- HashMap

instance FunctorWithIndex k (HM.HashMap k) where
  imap = HM.mapWithKey
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

-- IdentityT

instance FunctorWithIndex i m => FunctorWithIndex i (IdentityT m) where
  imap f (IdentityT m) = IdentityT $ imap f m
  {-# INLINE imap #-}

instance FoldableWithIndex i m => FoldableWithIndex i (IdentityT m) where
  ifoldMap f (IdentityT m) = ifoldMap f m
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i m => TraversableWithIndex i (IdentityT m) where
  itraverse f (IdentityT m) = IdentityT <$> itraverse f m
  {-# INLINE itraverse #-}

-- ReaderT

instance FunctorWithIndex i m => FunctorWithIndex (e, i) (ReaderT e m) where
  imap f (ReaderT m) = ReaderT $ \k -> imap (f . (,) k) (m k)
  {-# INLINE imap #-}
