-- |
-- Module: Optics.VL
--
-- This module provides compatibility layer for converting from/to van Laarhoven
-- encoding of 'Iso's, 'Prism's, 'Lens'es, 'IxLens'es, 'AffineTraversal's,
-- 'IxAffineTraversal's, 'Traversal's and 'IxTraversal's to their optics
-- equivalents.
module Optics.VL
  (
  -- * Iso
    IsoVL
  , IsoVL'
  , isoVL
  , toIsoVL
  , withIsoVL
  -- * Prism
  , PrismVL
  , PrismVL'
  , prismVL
  , toPrismVL
  , withPrismVL
  -- * Lens
  , LensVL
  , LensVL'
  , lensVL
  , toLensVL
  , withLensVL
  -- * IxLens
  , IxLensVL
  , IxLensVL'
  , ilensVL
  , toIxLensVL
  , withIxLensVL
  -- * AffineTraversal
  , AffineTraversalVL
  , AffineTraversalVL'
  , atraversalVL
  , atraverseOf
  -- * IxAffineTraversal
  , IxAffineTraversalVL
  , IxAffineTraversalVL'
  , iatraversalVL
  , iatraverseOf
  -- * Traversal
  , TraversalVL
  , TraversalVL'
  , traversalVL
  , traverseOf
  -- * IxTraversal
  , IxTraversalVL
  , IxTraversalVL'
  , itraversalVL
  , itraverseOf
  ) where

import Data.Coerce
import Data.Functor.Identity
import Data.Profunctor.Indexed ((.#), (#.))
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Indexed as IP

import Optics.Internal.Optic
import Optics.Core

newtype WrappedIxProfunctor p i a b =
  WrapIxProfunctor { unwrapIxProfunctor :: p i a b }

instance IP.Profunctor p => P.Profunctor (WrappedIxProfunctor p i) where
  dimap f g (WrapIxProfunctor piab) = WrapIxProfunctor (IP.dimap f g piab)
  lmap  f   (WrapIxProfunctor piab) = WrapIxProfunctor (IP.lmap  f   piab)
  rmap    g (WrapIxProfunctor piab) = WrapIxProfunctor (IP.rmap    g piab)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance IP.Choice p => P.Choice (WrappedIxProfunctor p i) where
  left'  (WrapIxProfunctor piab) = WrapIxProfunctor (IP.left'  piab)
  right' (WrapIxProfunctor piab) = WrapIxProfunctor (IP.right' piab)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

----------------------------------------

newtype WrappedProfunctor p f i a b =
  WrapProfunctor { unwrapProfunctor :: p a (f b) }

instance (P.Profunctor p, Functor f) => IP.Profunctor (WrappedProfunctor p f) where
  dimap f g (WrapProfunctor pafb) = WrapProfunctor (P.dimap f (fmap g) pafb)
  lmap  f   (WrapProfunctor pafb) = WrapProfunctor (P.lmap  f          pafb)
  rmap    g (WrapProfunctor pafb) = WrapProfunctor (P.rmap    (fmap g) pafb)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  lcoerce' = IP.lmap coerce
  rcoerce' = IP.rmap coerce
  {-# INLINE lcoerce' #-}
  {-# INLINE rcoerce' #-}

instance (P.Choice p, Applicative f) => IP.Choice (WrappedProfunctor p f) where
  left' (WrapProfunctor pafb) =
    WrapProfunctor (P.rmap (either (fmap Left) (pure . Right)) (P.left' pafb))
  right' (WrapProfunctor pafb) =
    WrapProfunctor (P.rmap (either (pure . Left) (fmap Right)) (P.right' pafb))
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
isoVL f = Optic $ IP.rcoerce @(Identity t) @t
                . (unwrapIxProfunctor #. f .# WrapIxProfunctor)
                . IP.rcoerce @b @(Identity b)
{-# INLINE isoVL #-}

-- | Convert an 'Iso' to the van Laarhoven representation.
toIsoVL :: Is k An_Iso => Optic k is s t a b -> IsoVL s t a b
toIsoVL o = unwrapProfunctor #. getOptic (castOptic @An_Iso o) .# WrapProfunctor
{-# INLINE toIsoVL #-}

-- | Work with an 'Iso' in the van Laarhoven representation.
withIsoVL
  :: Is k An_Iso
  => Optic k is s t a b
  -> (IsoVL s t a b -> r)
  -> r
withIsoVL o k = k (toIsoVL o)
{-# INLINE withIsoVL #-}

----------------------------------------

-- | Type synonym for a type-modifying van Laarhoven prism.
type PrismVL s t a b =
  forall p f. (P.Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | Type synonym for a type-preserving van Laarhoven prism.
type PrismVL' s a = PrismVL s s a a

-- | Build a 'Prism' from the van Laarhoven representation.
prismVL :: forall s t a b. PrismVL s t a b -> Prism s t a b
prismVL f = Optic $ IP.rcoerce @(Identity t) @t
                  . (unwrapIxProfunctor #. f .# WrapIxProfunctor)
                  . IP.rcoerce @b @(Identity b)
{-# INLINE prismVL #-}

-- | Convert a 'Prism' to the van Laarhoven representation.
toPrismVL :: Is k A_Prism => Optic k is s t a b -> PrismVL s t a b
toPrismVL o = unwrapProfunctor #. getOptic (castOptic @A_Prism o) .# WrapProfunctor
{-# INLINE toPrismVL #-}

-- | Work with a 'Prism' in the van Laarhoven representation.
withPrismVL
  :: Is k A_Prism
  => Optic k is s t a b
  -> (PrismVL s t a b -> r)
  -> r
withPrismVL o k = k (toPrismVL o)
{-# INLINE withPrismVL #-}
