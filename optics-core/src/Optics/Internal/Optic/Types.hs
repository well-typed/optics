{-# LANGUAGE TypeInType #-}
module Optics.Internal.Optic.Types where

import GHC.Exts (Constraint)
import Data.Kind (Type)

import Optics.Internal.Bi
import Optics.Internal.Profunctor

data OpticKind_
type OpticKind = OpticKind_ -> Type

-- | Tag for an equality.
data An_Equality :: OpticKind
-- | Tag for an iso.
data An_Iso :: OpticKind
-- | Tag for a lens.
data A_Lens :: OpticKind
-- | Tag for a prism.
data A_Prism :: OpticKind
-- | Tag for an affine traversal.
data An_AffineTraversal :: OpticKind
-- | Tag for a traversal.
data A_Traversal :: OpticKind
-- | Tag for an indexed traversal.
data An_IxTraversal :: OpticKind
-- | Tag for a setter.
data A_Setter :: OpticKind
-- | Tag for an indexed setter.
data An_IxSetter :: OpticKind
-- | Tag for a prismatic getter.
data A_PrismaticGetter :: OpticKind
-- | Tag for a getter.
data A_Getter :: OpticKind
-- | Tag for an affine fold.
data An_AffineFold :: OpticKind
-- | Tag for a fold.
data A_Fold :: OpticKind
-- | Tag for an indexed fold.
data An_IxFold :: OpticKind
-- | Tag for a lensy review.
data A_LensyReview :: OpticKind
-- | Tag for a review.
data A_Review :: OpticKind

-- | Mapping tag types @k@ to constraints on @p@.
--
-- Using this type family we define the constraints that the various flavours of
-- optics have to fulfill.
--
type family Constraints (k :: OpticKind) (p :: Type -> Type -> Type -> Type) :: Constraint where
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
