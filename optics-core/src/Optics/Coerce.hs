-- |
-- Module: Optics.Coerce
-- Description: Operators to 'coerce' the type parameters of 'Optic'.
--
-- This module defines operations to 'coerce' the type parameters of optics to
-- a representationally equal type.  For example, if we have
--
-- > newtype MkInt = MkInt Int
--
-- and
--
-- > l :: Lens' S Int
--
-- then
--
-- > coerceA @Int @MkInt l :: Lens' S MkInt
--
module Optics.Coerce
  ( coerceS
  , coerceT
  , coerceA
  , coerceB
  ) where

import Data.Coerce

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Lift 'coerce' to the @s@ parameter of an optic.
coerceS
  :: Coercible s s'
  => Optic k is s  t a b
  -> Optic k is s' t a b
coerceS = \(Optic o) -> Optic (lcoerce . o)
{-# INLINE coerceS #-}

-- | Lift 'coerce' to the @t@ parameter of an optic.
coerceT
  :: Coercible t t'
  => Optic k is s t  a b
  -> Optic k is s t' a b
coerceT = \(Optic o) -> Optic (rcoerce . o)
{-# INLINE coerceT #-}

-- | Lift 'coerce' to the @a@ parameter of an optic.
coerceA
  :: Coercible a a'
  => Optic k is s t a  b
  -> Optic k is s t a' b
coerceA = \(Optic o) -> Optic (o . lcoerce)
{-# INLINE coerceA #-}

-- | Lift 'coerce' to the @b@ parameter of an optic.
coerceB
  :: Coercible b b'
  => Optic k is s t a b
  -> Optic k is s t a b'
coerceB = \(Optic o) -> Optic (o . rcoerce)
{-# INLINE coerceB #-}
