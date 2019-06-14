{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Optics.VL
--
-- This module provides compatibility layer for converting from van Laarhoven
-- encoding of 'Iso's, 'Prism's, 'Lens'es, 'IxLens'es, 'AffineTraversal's,
-- 'IxAffineTraversal's, 'Traversal's and 'IxTraversal's to their optics
-- equivalents.
module Optics.VL
  (
  -- * Iso
    IsoVL
  , IsoVL'
  , isoVL
  -- * Prism
  , PrismVL
  , PrismVL'
  , prismVL
  -- * Lens
  , LensVL
  , LensVL'
  , lensVL
  -- * IxLens
  , IxLensVL
  , IxLensVL'
  , ixLensVL
  -- * AffineTraversal
  , AffineTraversalVL
  , AffineTraversalVL'
  , atraversalVL
  -- * IxAffineTraversal
  , IxAffineTraversalVL
  , IxAffineTraversalVL'
  , ixAtraversalVL
  -- * Traversal
  , TraversalVL
  , TraversalVL'
  , traversalVL
  -- * IxTraversal
  , IxTraversalVL
  , IxTraversalVL'
  , ixTraversalVL
  ) where

import Data.Functor.Identity
import qualified Data.Profunctor as P

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils
import Optics.Core

newtype WrappedProfunctor p i a b =
  WrapProfunctor { unwrapProfunctor :: p i a b }

instance Profunctor p => P.Profunctor (WrappedProfunctor p i) where
  dimap f g (WrapProfunctor pab) = WrapProfunctor (dimap f g pab)
  lmap  f   (WrapProfunctor pab) = WrapProfunctor (lmap  f   pab)
  rmap    g (WrapProfunctor pab) = WrapProfunctor (rmap    g pab)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice p => P.Choice (WrappedProfunctor p i) where
  left'  (WrapProfunctor pab) = WrapProfunctor (left'  pab)
  right' (WrapProfunctor pab) = WrapProfunctor (right' pab)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

----------------------------------------

-- | Type synonym for a type-modifying van Laarhoven iso.
type IsoVL s t a b =
  forall p f. (P.Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- | Type synonym for a type-preserving van Laarhoven iso.
type IsoVL' s a = IsoVL s s a a

-- | Build an 'Iso' from the van Laarhoven representation.
isoVL :: forall s t a b. IsoVL s t a b -> Iso s t a b
isoVL f = Optic $ rcoerce @(Identity t) @t
                . (unwrapProfunctor #. f .# WrapProfunctor)
                . rcoerce @b @(Identity b)
{-# INLINE isoVL #-}

----------------------------------------

-- | Type synonym for a type-modifying van Laarhoven prism.
type PrismVL s t a b =
  forall p f. (P.Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | Type synonym for a type-preserving van Laarhoven prism.
type PrismVL' s a = PrismVL s s a a

-- | Build a 'Prism' from the van Laarhoven representation.
prismVL :: forall s t a b. PrismVL s t a b -> Prism s t a b
prismVL f = Optic $ rcoerce @(Identity t) @t
                  . (unwrapProfunctor #. f .# WrapProfunctor)
                  . rcoerce @b @(Identity b)
{-# INLINE prismVL #-}
