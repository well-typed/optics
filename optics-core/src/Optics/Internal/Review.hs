module Optics.Internal.Review where

import Data.Void

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Tagged
import Optics.Internal.Utils

-- | Type synonym for a type-modifying review.
type Review s t a b = Optic A_Review NoIx s t a b

-- | Type synonym for a type-preserving review.
type Review' b t = Optic' A_Review NoIx t b

-- | Explicitly cast an optic to a review.
toReview :: Is k A_Review => Optic k is s t a b -> Optic A_Review is s t a b
toReview = sub
{-# INLINE toReview #-}

-- | Apply a review.
review :: Is k A_Review => Optic' k is t b -> b -> t
review o = unTagged #. getOptic (toReview o) .# Tagged
{-# INLINE review #-}

-- | An analogue of 'to' for review.
unto :: (b -> t) -> Review' b t
unto f = Optic (first absurd . dimap absurd f)
{-# INLINE unto #-}
