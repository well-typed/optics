-- |
-- Module: Optics.PrismaticGetter
-- Description: A backwards 'Optics.Prism.Prism'.
--
-- A 'PrismaticGetter' is a backwards 'Optics.Prism.Prism', i.e. a
-- @'PrismaticGetter' s t a b@ is equivalent to a @'Optics.Prism.Prism' b a t
-- s@.  These are typically produced by calling 'Optics.Re.re' on a
-- 'Optics.Prism.Prism'.  They are distinguished from a 'Optics.Getter.Getter'
-- so that @'Optics.Re.re' . 'Optics.Re.re'@ on a 'Optics.Prism.Prism' returns a
-- 'Optics.Prism.Prism'.
--
module Optics.PrismaticGetter
  ( -- * Formation
    PrismaticGetter
  , PrismaticGetter'

  -- * Introduction
  -- |
  --
  -- There is no canonical introduction form for 'PrismaticGetter', but you can
  -- use 'Optics.Re.re' to construct one from a 'Optics.Prism.Prism':
  --
  -- @
  -- (\\ f g -> 'Optics.Re.re' ('Optics.Prism.prism' f g)) :: (s -> a) -> (b -> Either a t) -> 'PrismaticGetter' s t a b
  -- @

  -- * Elimination
  -- |
  --
  -- A 'PrismaticGetter' is a 'Optics.Getter.Getter', so you can specialise
  -- types to obtain:
  --
  -- @
  -- 'Optics.Getter.view' :: 'PrismaticGetter'' s a -> s -> a
  -- @
  --
  -- There is no reversed 'Optics.AffineTraversal.matching' defined, but it is
  -- definable using 'Optics.Re.re':
  --
  -- @
  -- 'Optics.AffineTraversal.matching' . 'Optics.Re.re' :: 'PrismaticGetter' s t a b -> b -> Either a t
  -- @

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Getter.view'          $ 'Optics.Re.re' ('Optics.Prism.prism' f g) ≡ f
  -- 'Optics.AffineTraversal.matching' . 'Optics.Re.re' $ 'Optics.Re.re' ('Optics.Prism.prism' f g) ≡ g
  -- @

  -- * Subtyping
  , A_PrismaticGetter

  -- * Re-exports
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for a type-modifying prismatic getter.
type PrismaticGetter s t a b = Optic A_PrismaticGetter NoIx s t a b

-- | Type synonym for a type-preserving prismatic getter.
type PrismaticGetter' s a = Optic' A_PrismaticGetter NoIx s a
