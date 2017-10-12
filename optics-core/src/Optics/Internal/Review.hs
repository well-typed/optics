{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Review where

import Data.Bifunctor
import Data.Functor.Identity (Identity(..))

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a review.
data A_Review

-- | Constraints corresponding to a review.
type instance Constraints A_Review p f =
  (Choice p, Bifunctor p, f ~ Identity)

-- | Type synonym for a type-modifying review.
type Review s t a b = Optic A_Review s t a b

-- | Type synonym for a type-preserving review.
type Review' s a = Optic' A_Review s a

-- | Explicitly cast an optic to a review.
toReview :: Is k A_Review => Optic k s t a b -> Review s t a b
toReview = sub
{-# INLINE toReview #-}

-- | Build a review from the van Laarhoven representation.
mkReview :: (forall p . (Choice p, Bifunctor p) => p a (Identity b) -> p s (Identity t)) -> Review s t a b
mkReview = Optic
{-# INLINE mkReview #-}

