-- |
-- Module: Optics.LensyReview
-- Description: A backwards 'Optics.Lens.Lens'.
--
-- A 'LensyReview' is a backwards 'Optics.Lens.Lens', i.e. a @'LensyReview' s t
-- a b@ is equivalent to a @'Optics.Lens.Lens' b a t s@.  These are typically
-- produced by calling 'Optics.Re.re' on a 'Optics.Lens.Lens'.  They are
-- distinguished from a 'Optics.Review.Review' so that @'Optics.Re.re'
-- . 'Optics.Re.re'@ on a 'Optics.Lens.Lens' returns a 'Optics.Lens.Lens'.
--
module Optics.LensyReview
  (
  -- * Formation
    LensyReview
  , LensyReview'

  -- * Introduction
  -- |
  --
  -- There is no canonical introduction form for 'LensyReview', but you can use
  -- 'Optics.Re.re' to construct one from a 'Optics.Lens.Lens':
  --
  -- @
  -- (\\ f g -> 'Optics.Re.re' ('Optics.Lens.lens' f g)) :: (b -> t) -> (b -> s -> a) -> 'LensyReview' s t a b
  -- @

  -- * Elimination
  -- |
  --
  -- A 'LensyReview' is a 'Optics.Review.Review', so you can specialise types to obtain:
  --
  -- @
  -- 'Optics.Review.review' :: 'LensyReview'' s a -> a -> s
  -- @
  --
  -- There is no corresponding optic kind for a backwards
  -- 'Optics.Setter.Setter', but a reversed 'Optics.Setter.set' is definable
  -- using 'Optics.Re.re':
  --
  -- @
  -- 'Optics.Setter.set' . 'Optics.Re.re' :: 'LensyReview' s t a b -> s -> b -> a
  -- @

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Review.review'   $ 'Optics.Re.re' ('Optics.Lens.lens' f g) ≡ f
  -- 'Optics.Setter.set' . 'Optics.Re.re' $ 'Optics.Re.re' ('Optics.Lens.lens' f g) ≡ g
  -- @

  -- * Subtyping
  , A_LensyReview

  -- * Re-exports
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for a type-modifying lensy review.
type LensyReview s t a b = Optic A_LensyReview NoIx s t a b

-- | Type synonym for a type-preserving lensy review.
type LensyReview' t b = Optic' A_LensyReview NoIx t b
