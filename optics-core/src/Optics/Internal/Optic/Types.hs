{-# OPTIONS_HADDOCK not-home #-}
module Optics.Internal.Optic.Types where

import GHC.Exts (Constraint)

import Optics.Internal.Bi
import Optics.Internal.Profunctor

-- | Tag for an equality.
data An_Equality
-- | Tag for an iso.
data An_Iso
-- | Tag for a lens.
data A_Lens
-- | Tag for a prism.
data A_Prism
-- | Tag for an affine traversal.
data An_AffineTraversal
-- | Tag for a traversal.
data A_Traversal
-- | Tag for a setter.
data A_Setter
-- | Tag for a prismatic getter.
data A_PrismaticGetter
-- | Tag for a getter.
data A_Getter
-- | Tag for an affine fold.
data An_AffineFold
-- | Tag for a fold.
data A_Fold
-- | Tag for a lensy review.
data A_LensyReview
-- | Tag for a review.
data A_Review

-- | Mapping tag types @k@ to constraints on @p@.
--
-- Using this type family we define the constraints that the various flavours of
-- optics have to fulfill.
--
type family Constraints (k :: *) (p :: * -> * -> * -> *) :: Constraint where
  Constraints An_Equality        p = ()
  Constraints An_Iso             p = Profunctor p
  Constraints A_Lens             p = Strong p
  Constraints A_LensyReview      p = Costrong p
  Constraints A_Prism            p = Choice p
  Constraints A_PrismaticGetter  p = Cochoice p
  Constraints An_AffineTraversal p = Visiting p
  Constraints A_Traversal        p = Traversing p
  Constraints A_Setter           p = Mapping p
  Constraints A_Getter           p = (Bicontravariant p, Cochoice p, Strong p)
  Constraints An_AffineFold      p = (Bicontravariant p, Cochoice p, Visiting p)
  Constraints A_Fold             p = (Bicontravariant p, Cochoice p, Traversing p)
  Constraints A_Review           p = (Bifunctor p, Choice p, Costrong p)
