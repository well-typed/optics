-- |
-- Module: Optics.LensyReview
-- Description: A 'Optics.Review.Review' produced by 'Optics.Re.re' on a 'Optics.Lens.Lens'.
--
-- A 'LensyReview' is a 'Optics.Review.Review' produced by calling
-- 'Optics.Re.re' on a 'Optics.Lens.Lens'.  It is essentially
-- equivalent to a 'Optics.Review.Review', and is distinguished merely
-- so that @'Optics.Re.re' . 'Optics.Re.re'@ on a 'Optics.Lens.Lens'
-- returns a 'Optics.Lens.Lens'.
--
module Optics.LensyReview
  (
  -- * Formation
    LensyReview
  , LensyReview'

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
