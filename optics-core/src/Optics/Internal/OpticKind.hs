module Optics.Internal.OpticKind
  ( OpticKind (..)
 ) where

-- The type is defined here, so GHC messages refer to exposed module in all cases.

-- | Optic kinds.
data OpticKind
      -- | Tag for an equality.
  =  An_Equality
  -- | Tag for an iso.
  |  An_Iso
  -- | Tag for a lens.
  |  A_Lens
  -- | Tag for a prism.
  |  A_Prism
  -- | Tag for an affine traversal.
  |  An_AffineTraversal
  -- | Tag for a traversal.
  |  A_Traversal
  -- | Tag for an indexed traversal.
  |  An_IxTraversal
  -- | Tag for a setter.
  |  A_Setter
  -- | Tag for an indexed setter.
  |  An_IxSetter
  -- | Tag for a prismatic getter.
  |  A_PrismaticGetter
  -- | Tag for a getter.
  |  A_Getter
  -- | Tag for an affine fold.
  |  An_AffineFold
  -- | Tag for a fold.
  |  A_Fold
  -- | Tag for an indexed fold.
  |  An_IxFold
  -- | Tag for a lensy review.
  |  A_LensyReview
  -- | Tag for a review.
  |  A_Review
