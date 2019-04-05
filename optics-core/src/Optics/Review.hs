-- |
-- Module: Optics.Review
-- Description: A backwards 'Optics.Getter.Getter', i.e. a function.
--
-- A 'Review' is a backwards 'Optics.Getter.Getter', i.e. a
-- @'Review' T B@ is just a function @B -> T@.
--
module Optics.Review
  (
  -- * Formation
    Review

  -- * Introduction
  , unto

  -- * Elimination
  , review

  -- * Computation
  -- |
  --
  -- @
  -- 'review' ('unto' f) = f
  -- @

  -- * Subtyping
  , A_Review
  , toReview

  -- * Re-exports
  , module Optics.Optic
  )
  where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Tagged
import Optics.Internal.Utils
import Optics.Optic

-- | Type synonym for a review.
type Review t b = Optic' A_Review NoIx t b

-- | Explicitly cast an optic to a review.
toReview :: Is k A_Review => Optic k is s t a b -> Optic A_Review is s t a b
toReview = castOptic
{-# INLINE toReview #-}

-- | Retrieve the value targeted by a 'Review'.
--
-- >>> review _Left "hi"
-- Left "hi"
review :: Is k A_Review => Optic' k is t b -> b -> t
review o = unTagged #. getOptic (toReview o) .# Tagged
{-# INLINE review #-}

-- | An analogue of 'Optics.Getter.to' for reviews.
unto :: (b -> t) -> Review t b
unto f = Optic (lphantom . rmap f)
{-# INLINE unto #-}
