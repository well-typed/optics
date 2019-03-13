{-# LANGUAGE DataKinds #-}
-- | An 'IxSetter' is an indexed version of an 'Optics.Setter.Setter'.
-- See "Optics.Indexed.Core" for a discussion of indexed optics in
-- general.
--
module Optics.IxSetter
  (
  -- * Formation
    IxSetter
  , IxSetter'

  -- * Introduction
  , isets
  , imapped

  -- * Elimination
  , iset
  , iset'
  , iover
  , iover'

  -- * Subtyping
  , A_Setter
  , toIxSetter

  -- * Re-exports
  , FunctorWithIndex(..)
  , module Optics.Optic
  ) where

import Optics.Internal.Indexed
import Optics.Internal.IxSetter
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a type-modifying indexed setter.
type IxSetter i s t a b = Optic A_Setter (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed setter.
type IxSetter' i s a = Optic' A_Setter (WithIx i) s a

-- | Explicitly cast an optic to an indexeed setter.
toIxSetter
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxSetter i s t a b
toIxSetter = castOptic
{-# INLINE toIxSetter #-}

-- | Apply an indexed setter as a modifier.
iover
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover o = \f -> runIxFunArrow (getOptic (toIxSetter o) (IxFunArrow f)) id
{-# INLINE iover #-}

-- | Apply an indexed setter as a modifier, strictly.
iover'
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover' o = \f ->
  let star = getOptic (toIxSetter o) $ IxStar (\i -> wrapIdentity' . f i)
  in unwrapIdentity' . runIxStar star id

{-# INLINE iover' #-}

-- | Apply an indexed setter.
iset
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> b) -> s -> t
iset o = \f -> iover o (\i _ -> f i)
{-# INLINE iset #-}

-- | Apply an indexed setter, strictly.
iset'
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> b) -> s -> t
iset' o = \f -> iover' o (\i _ -> f i)
{-# INLINE iset' #-}

-- | Build an indexed setter from a function to modify the element(s).
isets
  :: ((i -> a -> b) -> s -> t)
  -> IxSetter i s t a b
isets f = Optic (iroam f)
{-# INLINE isets #-}

-- | Indexed setter via the 'FunctorWithIndex' class.
imapped :: FunctorWithIndex i f => IxSetter i (f a) (f b) a b
imapped = Optic imapped__
{-# INLINE imapped #-}
