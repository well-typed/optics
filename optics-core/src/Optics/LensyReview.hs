module Optics.LensyReview
  ( A_LensyReview
  , LensyReview
  , LensyReview'
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for a type-modifying lensy review.
type LensyReview s t a b = Optic A_LensyReview NoIx s t a b

-- | Type synonym for a type-preserving lensy review.
type LensyReview' t b = Optic' A_LensyReview NoIx t b
