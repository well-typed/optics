{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Review where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a review.
data A_Review

-- | Constraints corresponding to a review.
type instance Constraints A_Review p = InPhantom p

-- | Type synonym for a type-modifying review.
type Review s t a b = Optic A_Review s t a b

-- | Type synonym for a type-preserving review.
type Review' s a = Optic' A_Review s a

-- | Explicitly cast an optic to a review.
toReview :: Is k A_Review => Optic k s t a b -> Review s t a b
toReview = sub
{-# INLINE toReview #-}

-- | Create a review.
mkReview :: Optic_ A_Review s t a b -> Review s t a b
mkReview = Optic
{-# INLINE mkReview #-}
