module Optics.LensyReview
  ( A_LensyReview
  , LensyReview
  , LensyReview'
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for a type-modifying lensy review.
type LensyReview i s t a b = Optic A_LensyReview i i s t a b

-- | Type synonym for a type-preserving lensy review.
type LensyReview' i b t = Optic' A_LensyReview i i t b
