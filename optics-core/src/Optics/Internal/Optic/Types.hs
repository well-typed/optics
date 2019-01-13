module Optics.Internal.Optic.Types
  ( OpticKind
  , An_Equality
  , An_Iso
  , A_Lens
  , A_Prism
  , An_AffineTraversal
  , A_Traversal
  , An_IxTraversal
  , A_Setter
  , An_IxSetter
  , A_PrismaticGetter
  , A_Getter
  , An_AffineFold
  , A_Fold
  , An_IxFold
  , A_LensyReview
  , A_Review
  , Constraints
  ) where

import GHC.Exts (Constraint)

import Optics.Internal.Bi
import Optics.Internal.Profunctor

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

-- | Tag for an equality.
type An_Equality = 'An_Equality
-- | Tag for an iso.
type An_Iso = 'An_Iso
-- | Tag for a lens.
type A_Lens = 'A_Lens
-- | Tag for a prism.
type A_Prism = 'A_Prism
-- | Tag for an affine traversal.
type An_AffineTraversal = 'An_AffineTraversal
-- | Tag for a traversal.
type A_Traversal = 'A_Traversal
-- | Tag for an indexed traversal.
type An_IxTraversal = 'An_IxTraversal
-- | Tag for a setter.
type A_Setter = 'A_Setter
-- | Tag for an indexed setter.
type An_IxSetter = 'An_IxSetter
-- | Tag for a prismatic getter.
type A_PrismaticGetter = 'A_PrismaticGetter
-- | Tag for a getter.
type A_Getter = 'A_Getter
-- | Tag for an affine fold.
type An_AffineFold = 'An_AffineFold
-- | Tag for a fold.
type A_Fold = 'A_Fold
-- | Tag for an indexed fold.
type An_IxFold = 'An_IxFold
-- | Tag for a lensy review.
type A_LensyReview = 'A_LensyReview
-- | Tag for a review.
type A_Review = 'A_Review

-- | Mapping tag types @k@ to constraints on @p@.
--
-- Using this type family we define the constraints that the various flavours of
-- optics have to fulfill.
--
type family Constraints (k :: OpticKind) (p :: * -> * -> * -> *) :: Constraint where
  Constraints An_Equality        p = ()
  Constraints An_Iso             p = Profunctor p
  Constraints A_Lens             p = Strong p
  Constraints A_LensyReview      p = Costrong p
  Constraints A_Prism            p = Choice p
  Constraints A_PrismaticGetter  p = Cochoice p
  Constraints An_AffineTraversal p = (Strong p, Choice p)
  Constraints A_Traversal        p = IxTraversing p
  Constraints An_IxTraversal     p = IxTraversing p
  Constraints A_Setter           p = IxMapping p
  Constraints An_IxSetter        p = IxMapping p
  Constraints A_Getter           p = (Bicontravariant p, Cochoice p, Strong p)
  Constraints An_AffineFold      p = (Bicontravariant p, Cochoice p, Strong p, Choice p)
  Constraints A_Fold             p = (Bicontravariant p, Cochoice p, IxTraversing p)
  Constraints An_IxFold          p = (Bicontravariant p, Cochoice p, IxTraversing p)
  Constraints A_Review           p = (Bifunctor p, Choice p, Costrong p)
