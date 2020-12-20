-- |
-- Module: Optics.ReversedPrism
-- Description: A backwards 'Optics.Prism.Prism'.
--
-- A 'ReversedPrism' is a backwards 'Optics.Prism.Prism', i.e. a
-- @'ReversedPrism' s t a b@ is equivalent to a @'Optics.Prism.Prism' b a t
-- s@.  These are typically produced by calling 'Optics.Re.re' on a
-- 'Optics.Prism.Prism'.  They are distinguished from a 'Optics.Getter.Getter'
-- so that @'Optics.Re.re' . 'Optics.Re.re'@ on a 'Optics.Prism.Prism' returns a
-- 'Optics.Prism.Prism'.
--
module Optics.ReversedPrism
  ( -- * Formation
    ReversedPrism
  , ReversedPrism'

  -- * Introduction
  -- |
  --
  -- There is no canonical introduction form for 'ReversedPrism', but you can
  -- use 'Optics.Re.re' to construct one from a 'Optics.Prism.Prism':
  --
  -- @
  -- (\\ f g -> 'Optics.Re.re' ('Optics.Prism.prism' f g)) :: (s -> a) -> (b -> Either a t) -> 'ReversedPrism' s t a b
  -- @

  -- * Elimination
  -- |
  --
  -- A 'ReversedPrism' is a 'Optics.Getter.Getter', so you can specialise
  -- types to obtain:
  --
  -- @
  -- 'Optics.Getter.view' :: 'ReversedPrism'' s a -> s -> a
  -- @
  --
  -- There is no reversed 'Optics.AffineTraversal.matching' defined, but it is
  -- definable using 'Optics.Re.re':
  --
  -- @
  -- 'Optics.AffineTraversal.matching' . 'Optics.Re.re' :: 'ReversedPrism' s t a b -> b -> Either a t
  -- @

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Getter.view'          $ 'Optics.Re.re' ('Optics.Prism.prism' f g) ≡ f
  -- 'Optics.AffineTraversal.matching' . 'Optics.Re.re' $ 'Optics.Re.re' ('Optics.Prism.prism' f g) ≡ g
  -- @

  -- * Subtyping
  , A_ReversedPrism
  -- | <<diagrams/ReversedPrism.png ReversedPrism in the optics hierarchy>>
  ) where

import Optics.Internal.Optic

-- | Type synonym for a type-modifying reversed prism.
type ReversedPrism s t a b = Optic A_ReversedPrism 'NoIx s t a b

-- | Type synonym for a type-preserving reversed prism.
type ReversedPrism' s a = Optic' A_ReversedPrism 'NoIx s a
