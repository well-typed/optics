{-# LANGUAGE DataKinds #-}
-- |
-- Module: Optics.IxSetter
-- Description: An indexed version of a 'Optics.Setter.Setter'.
--
-- An 'IxSetter' is an indexed version of a 'Optics.Setter.Setter'. See the
-- "Indexed optics" section of the overview documentation in the @Optics@ module
-- of the main @optics@ package for more details on indexed optics.
--
module Optics.IxSetter
  (
  -- * Formation
    IxSetter
  , IxSetter'

  -- * Introduction
  , isets

  -- * Elimination
  , iover

  -- * Computation
  -- |
  --
  -- @
  -- 'iover' ('isets' f) ≡ f
  -- @

  -- * Well-formedness
  -- |
  --
  -- * __PutPut__: Setting twice is the same as setting once:
  --
  --     @
  --     'Optics.Setter.iset' l v' ('Optics.Setter.iset' l v s) ≡ 'Optics.Setter.iset' l v' s
  --     @
  --
  -- * __Functoriality__: 'IxSetter's must preserve identities and composition:
  --
  --     @
  --     'iover' s ('const' 'id') ≡ 'id'
  --     'iover' s f '.' 'iover' s g ≡ 'iover' s (\i -> f i '.' g i)
  --     @

  -- * Additional introduction forms
  , imapped

  -- * Additional elimination forms
  , iset
  , iset'
  , iover'

  -- * Subtyping
  , A_Setter

  -- * Re-exports
  , FunctorWithIndex(..)
  ) where

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Indexed.Classes
import Optics.Internal.IxSetter
import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Type synonym for a type-modifying indexed setter.
type IxSetter i s t a b = Optic A_Setter ('WithIx i) s t a b

-- | Type synonym for a type-preserving indexed setter.
type IxSetter' i s a = Optic' A_Setter ('WithIx i) s a

-- | Apply an indexed setter as a modifier.
iover
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover o = \f -> runIxFunArrow (getOptic (castOptic @A_Setter o) (IxFunArrow f)) id
{-# INLINE iover #-}

-- | Apply an indexed setter as a modifier, strictly.
iover'
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover' o = \f ->
  let star = getOptic (castOptic @A_Setter o) $ IxStar (\i -> wrapIdentity' . f i)
  in unwrapIdentity' . runIxStar star id

{-# INLINE iover' #-}

-- | Apply an indexed setter.
--
-- @
-- 'iset' o f ≡ 'iover' o (\i _ -> f i)
-- @
--
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
--
-- @
-- 'iover' 'imapped' ≡ 'imap'
-- @
imapped :: FunctorWithIndex i f => IxSetter i (f a) (f b) a b
imapped = Optic imapped__
{-# INLINE imapped #-}
