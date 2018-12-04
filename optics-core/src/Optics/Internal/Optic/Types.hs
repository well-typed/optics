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
-- | Tag for an indexed traversal.
data An_IxTraversal
-- | Tag for a setter.
data A_Setter
-- | Tag for an indexed setter.
data An_IxSetter
-- | Tag for a prismatic getter.
data A_PrismaticGetter
-- | Tag for a getter.
data A_Getter
-- | Tag for an affine fold.
data An_AffineFold
-- | Tag for a fold.
data A_Fold
-- | Tag for an indexed fold.
data An_IxFold
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
  Constraints An_AffineTraversal p = (Strong p, Choice p)
  Constraints A_Traversal        p = Traversing p
  Constraints An_IxTraversal     p = TraversingWithIndex p
  Constraints A_Setter           p = Mapping p
  Constraints An_IxSetter        p = p ~ IxFunArrow
  Constraints A_Getter           p = (Bicontravariant p, Cochoice p, Strong p)
  Constraints An_AffineFold      p = (Bicontravariant p, Cochoice p, Strong p, Choice p)
  Constraints A_Fold             p = (Bicontravariant p, Cochoice p, Traversing p)
  Constraints An_IxFold          p = (Bicontravariant p, Cochoice p, TraversingWithIndex p)
  Constraints A_Review           p = (Bifunctor p, Choice p, Costrong p)
