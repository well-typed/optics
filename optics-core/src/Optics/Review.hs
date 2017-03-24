{-# LANGUAGE FlexibleContexts #-}
module Optics.Review
  ( A_Review
  , Review
  , Review'
  , toReview
  , mkReview
  , re
  , review
  , module Optics.Optic
  )
  where

import Optics.Internal.Getter
import Optics.Internal.Review
import Optics.Internal.Optic
import Optics.Internal.Tagged
import Optics.Optic

-- | A review can be used as a getter from the small to the big type.
re :: Is k A_Review => Optic' k s a -> Getter a s
re o = to (unTagged . getOptic (toReview o) . Tagged)

-- | Apply a review.
review :: Is k A_Review => Optic' k s a -> a -> s
review = view . re
