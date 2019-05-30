-- |
-- Module: Optics.IxLens
-- Description: An indexed version of an 'Optics.Lens.Lens'.
--
-- An 'IxLens' is an indexed version of an 'Optics.Lens.Lens'. See the "Indexed
-- optics" section of the overview documentation in the @Optics@ module of the
-- main @optics@ package for more details on indexed optics.
--
module Optics.IxLens
  (
  -- * Formation
    IxLens
  , IxLens'

  -- * Van Laarhoven representation
  , IxLensVL
  , IxLensVL'
  , ixLensVL
  , toIxLensVL

  -- * Subtyping
  , A_Lens

  -- * Re-exports
  , module Optics.Optic
  ) where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a type-modifying indexed lens.
type IxLens i s t a b = Optic A_Lens (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed lens.
type IxLens' i s a = Optic' A_Lens (WithIx i) s a

-- | Type synonym for a type-modifying van Laarhoven indexed lens.
type IxLensVL i s t a b =
  forall f. Functor f => (i -> a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven indexed lens.
type IxLensVL' i s a = IxLensVL i s s a a

-- | Build an indexed lens from the van Laarhoven representation.
ixLensVL :: IxLensVL i s t a b -> IxLens i s t a b
ixLensVL f = Optic (ilinear f)
{-# INLINE ixLensVL #-}

-- | Convert an indexed lens to its van Laarhoven representation.
toIxLensVL
  :: (Is k A_Lens, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxLensVL i s t a b
toIxLensVL o = \f ->
  runIxStar (getOptic (castOptic @A_Lens o) (IxStar f)) id
{-# INLINE toIxLensVL #-}
