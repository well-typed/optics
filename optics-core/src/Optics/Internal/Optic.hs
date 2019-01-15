{-# LANGUAGE TypeInType #-}
-- | Core optic types and subtyping machinery.
--
-- This module contains the core 'Optic' types, and the underlying
-- machinery that we need in order to implement the subtyping between
-- various different flavours of optics.
--
-- The composition operator for optics is also defined here.
--
module Optics.Internal.Optic
  ( Optic(..)
  , Optic'
  , Optic_
  , Optic__
  , sub
  , (%)
  , (%%)
  , module Optics.Internal.Optic.Subtyping
  , module Optics.Internal.Optic.Types
  ) where

import Data.Kind (Type)
import Optics.Internal.Optic.Subtyping
import Optics.Internal.Optic.Types

-- | Wrapper newtype for the whole family of vaguely lens-like things.
--
-- The first type parameter @k@ identifies the particular flavour
-- (e.g. 'A_Lens' or 'A_Traversal').
--
-- The type parameters @s@ and @t@ represent the "big" structure,
-- whereas @a@ and @b@ represent the "small" structure.
--
-- TODO: explain indices
--
newtype Optic k i o s t a b =
  Optic { getOptic :: forall p. Optic_ k p i o s t a b }

-- | Common special case of 'Optic' where source and target types are equal.
--
-- Here, we need only one "big" and one "small" type. For lenses, this
-- means that in the restricted form we cannot do type-changing updates.
--
type Optic' k i o s a = Optic k i o s s a a

-- | Type representing the various kinds of optics.
--
-- The tag parameter @k@ is translated into constraints on @p@
-- via the type family 'Constraints'.
--
type Optic_ k p i o s t a b = Constraints k p => Optic__ p i o s t a b

-- | Optic internally as a profunctor transformation.
type Optic__ p i o s t a b = p i a b -> p o s t

-- | Proxy type for use as an argument to 'implies'.
--
data IsProxy (k :: OpticKind) (l :: OpticKind) (p :: Type -> Type -> Type -> Type) =
  IsProxy

-- | Explicit cast from one optic flavour to another.
--
-- This is the identity function, modulo some constraint jiggery-pokery.
--
-- TODO: add a graph
--
sub :: forall k l i o s t a b . Is k l => Optic k i o s t a b -> Optic l i o s t a b
sub (Optic o) = Optic (implies' o)
  where
    implies' :: forall p . Optic_ k p i o s t a b -> Optic_ l p i o s t a b
    implies' x = implies (IsProxy :: IsProxy k l p) x
{-# INLINE sub #-}

-- | Compose two optics of compatible flavours.
--
-- Returns an optic of the appropriate supertype.
--
(%) :: (Is k m, Is l m, m ~ Join k l)
    => Optic k j o s t u v
    -> Optic l i j u v a b
    -> Optic m i o s t a b
o % o' = sub o %% sub o'
{-# INLINE (%) #-}

-- | Compose two optics of the same flavour.
(%%) :: Optic k j o s t u v -> Optic k i j u v a b -> Optic k i o s t a b
Optic o %% Optic o' = Optic (o . o')
{-# INLINE (%%) #-}
