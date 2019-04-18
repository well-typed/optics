-- |
-- Module: Optics.PrismaticGetter
-- Description: A 'Optics.Getter.Getter' produced by 'Optics.Re.re' on a 'Optics.Prism.Prism'.
--
-- A 'PrismaticGetter' is a 'Optics.Getter.Getter' produced by
-- calling 'Optics.Re.re' on a 'Optics.Prism.Prism'.  It is
-- essentially equivalent to a 'Optics.Getter.Getter', and is
-- distinguished merely so that @'Optics.Re.re' . 'Optics.Re.re'@ on a
-- 'Optics.Prism.Prism' returns a 'Optics.Prism.Prism'.
--
module Optics.PrismaticGetter
  ( -- * Formation
    PrismaticGetter
  , PrismaticGetter'
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
