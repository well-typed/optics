-- |
-- Module: Optics.IxLens
-- Description: An indexed version of a 'Optics.Lens.Lens'.
--
-- An 'IxLens' is an indexed version of a 'Optics.Lens.Lens'. See the "Indexed
-- optics" section of the overview documentation in the @Optics@ module of the
-- main @optics@ package for more details on indexed optics.
--
module Optics.IxLens
  (
  -- * Formation
    IxLens
  , IxLens'

  -- * Introduction
  , ilens

  -- * Elimination
  -- | An 'IxLens' is in particular an 'Optics.IxGetter.IxGetter' and an
  -- 'Optics.IxSetter.IxSetter', therefore you can specialise types to obtain:
  --
  -- @
  -- 'Optics.IxGetter.iview' :: 'IxLens' i s t a b -> s -> (i, a)
  -- @
  --
  -- @
  -- 'Optics.IxSetter.iover' :: 'IxLens' i s t a b -> (i -> a -> b) -> s -> t
  -- 'Optics.IxSetter.iset'  :: 'IxLens' i s t a b -> (i      -> b) -> s -> t
  -- @

  -- * Additional introduction forms
  , devoid

  -- * Subtyping
  , A_Lens

  -- * van Laarhoven encoding
  , IxLensVL
  , IxLensVL'
  , ilensVL
  , toIxLensVL
  , withIxLensVL
  ) where

import Data.Void

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Optic

-- | Type synonym for a type-modifying indexed lens.
type IxLens i s t a b = Optic A_Lens (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed lens.
type IxLens' i s a = Optic' A_Lens (WithIx i) s a

-- | Type synonym for a type-modifying van Laarhoven indexed lens.
type IxLensVL i s t a b =
  forall f. Functor f => (i -> a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven indexed lens.
type IxLensVL' i s a = IxLensVL i s s a a

-- | Build an indexed lens from a getter and a setter.
--
-- If you want to build an 'IxLens' from the van Laarhoven representation, use
-- 'ilensVL'.
ilens :: (s -> (i, a)) -> (s -> b -> t) -> IxLens i s t a b
ilens get set = ilensVL $ \f s -> set s <$> uncurry f (get s)
{-# INLINE ilens #-}

-- | Build an indexed lens from the van Laarhoven representation.
ilensVL :: IxLensVL i s t a b -> IxLens i s t a b
ilensVL f = Optic (ilinear f)
{-# INLINE ilensVL #-}

-- | Convert an indexed lens to its van Laarhoven representation.
toIxLensVL
  :: (Is k A_Lens, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxLensVL i s t a b
toIxLensVL o = \f ->
  runIxStar (getOptic (castOptic @A_Lens o) (IxStar f)) id
{-# INLINE toIxLensVL #-}

-- | Work with an indexed lens in the van Laarhoven representation.
withIxLensVL
  :: (Is k A_Lens, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (IxLensVL i s t a b -> r)
  -> r
withIxLensVL o k = k (toIxLensVL o)
{-# INLINE withIxLensVL #-}

----------------------------------------
-- Lenses

-- | There is an indexed field for every type in the 'Void'.
--
-- >>> set (mapped % devoid) 1 []
-- []
--
-- >>> over (_Just % devoid) abs Nothing
-- Nothing
--
devoid :: IxLens' i Void a
devoid = ilens absurd const
{-# INLINE devoid #-}

-- $setup
-- >>> import Optics.Core
