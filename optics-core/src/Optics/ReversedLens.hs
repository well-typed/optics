-- |
-- Module: Optics.ReversedLens
-- Description: A backwards 'Optics.Lens.Lens'.
--
-- A 'ReversedLens' is a backwards 'Optics.Lens.Lens', i.e. a @'ReversedLens' s t
-- a b@ is equivalent to a @'Optics.Lens.Lens' b a t s@.  These are typically
-- produced by calling 'Optics.Re.re' on a 'Optics.Lens.Lens'.  They are
-- distinguished from a 'Optics.Review.Review' so that @'Optics.Re.re'
-- . 'Optics.Re.re'@ on a 'Optics.Lens.Lens' returns a 'Optics.Lens.Lens'.
--
module Optics.ReversedLens
  (
  -- * Formation
    ReversedLens
  , ReversedLens'

  -- * Introduction
  -- |
  --
  -- There is no canonical introduction form for 'ReversedLens', but you can use
  -- 'Optics.Re.re' to construct one from a 'Optics.Lens.Lens':
  --
  -- @
  -- (\\ f g -> 'Optics.Re.re' ('Optics.Lens.lens' f g)) :: (b -> t) -> (b -> s -> a) -> 'ReversedLens' s t a b
  -- @

  -- * Elimination
  -- |
  --
  -- A 'ReversedLens' is a 'Optics.Review.Review', so you can specialise types to obtain:
  --
  -- @
  -- 'Optics.Review.review' :: 'ReversedLens'' s a -> a -> s
  -- @
  --
  -- There is no corresponding optic kind for a backwards
  -- 'Optics.Setter.Setter', but a reversed 'Optics.Setter.set' is definable
  -- using 'Optics.Re.re':
  --
  -- @
  -- 'Optics.Setter.set' . 'Optics.Re.re' :: 'ReversedLens' s t a b -> s -> b -> a
  -- @

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Review.review'   $ 'Optics.Re.re' ('Optics.Lens.lens' f g) ≡ f
  -- 'Optics.Setter.set' . 'Optics.Re.re' $ 'Optics.Re.re' ('Optics.Lens.lens' f g) ≡ g
  -- @

  -- * Subtyping
  , A_ReversedLens
  -- | <<diagrams/ReversedLens.png ReversedLens in the optics hierarchy>>

  -- * Re-exports
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for a type-modifying reversed lens.
type ReversedLens s t a b = Optic A_ReversedLens NoIx s t a b

-- | Type synonym for a type-preserving reversed lens.
type ReversedLens' t b = Optic' A_ReversedLens NoIx t b
