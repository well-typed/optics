{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Optic.Types where

import Data.Kind (Constraint, Type)

import Data.Profunctor.Indexed

import Optics.Internal.Bi

-- | Kind for types used as optic tags, such as 'A_Lens'.
--
-- @since 0.2
type OpticKind = Type

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
-- | Tag for a setter.
data A_Setter :: OpticKind
-- | Tag for a reversed prism.
data A_ReversedPrism :: OpticKind
-- | Tag for a getter.
data A_Getter :: OpticKind
-- | Tag for an affine fold.
data An_AffineFold :: OpticKind
-- | Tag for a fold.
data A_Fold :: OpticKind
-- | Tag for a reversed lens.
data A_ReversedLens :: OpticKind
-- | Tag for a review.
data A_Review :: OpticKind

-- | Mapping tag types @k@ to constraints on @p@.
--
-- Using this type family we define the constraints that the various flavours of
-- optics have to fulfill.
--
type family Constraints (k :: OpticKind) (p :: Type -> Type -> Type -> Type) :: Constraint where
  Constraints An_Iso             p = Profunctor p
  Constraints A_Lens             p = Strong p
  Constraints A_ReversedLens     p = Costrong p
  Constraints A_Prism            p = Choice p
  Constraints A_ReversedPrism    p = Cochoice p
  Constraints An_AffineTraversal p = Visiting p
  Constraints A_Traversal        p = Traversing p
  Constraints A_Setter           p = Mapping p
  Constraints A_Getter           p = (Bicontravariant p, Cochoice p, Strong p)
  Constraints An_AffineFold      p = (Bicontravariant p, Cochoice p, Visiting p)
  Constraints A_Fold             p = (Bicontravariant p, Cochoice p, Traversing p)
  Constraints A_Review           p = (Bifunctor p, Choice p, Costrong p)
