{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core optic types and subtyping machinery.
--
-- This module contains the core 'Optic' types, and the underlying
-- machinery that we need in order to implement the subtyping between
-- various different flavours of optics.
--
-- The composition operator for optics is also defined here.
--
module Optics.Internal.Optic where

import GHC.Exts (Constraint)

-- | Wrapper newtype for the whole family of vaguely lens-like things.
--
-- The first type parameter @k@ identifies the particular flavour
-- (e.g. 'A_Lens' or 'A_Traversal').
--
-- The type parameters @s@ and @t@ represent the "big" structure,
-- whereas @a@ and @b@ represent the "small" structure.
--
newtype Optic k s t a b =
  Optic { getOptic :: Optic_ k s t a b }

-- | Type representing the various kinds of optics.
--
-- This quantifies over the variables @p@ and @f@. The constraints on
-- @p@ and @f@ are represented by the tag type @k@.
--
type Optic_ k s t a b =
  forall p f . Optic__ k p f s t a b

-- | Type representing the various kinds of optics.
--
-- The tag parameter @k@ is translated into constraints on @p@ and @f@
-- via the type family 'Constraints'.
--
type Optic__ k p f s t a b =
  Constraints k p f => p a (f b) -> p s (f t)

-- | Common special case of 'Optic' where source and target types are equal.
--
-- Here, we need only one "big" and one "small" type. For lenses, this
-- means that in the restricted form we cannot do type-changing updates.
--
type Optic' k s a = Optic k s s a a

-- | Common special case of 'Optic_' where source and target types are equal.
type Optic_' k s a = Optic_ k s s a a

-- | Mapping tag types @k@ to constraints on @p@ and @f@.
--
-- Using this open type family, we define the constraints that the
-- various flavours of optics have to fulfill.
--
type family Constraints
  (k :: *) (p :: * -> * -> *) (f :: * -> *) :: Constraint

-- | Subtyping relationship between flavours of optics.
--
-- An instance of @Is k l@ represents that any @Optic k@ can be used as
-- an @Optic l@. For example, we have an @Is A_Lens A_Traversal@ instance,
-- but not @Is A_Traversal A_Lens@.
--
-- This class needs instances for all possible combinations of tags.
--
class Is k l where
  -- | Witness of the subtyping relationship.
  implies ::
    proxy k l p f -> (Constraints k p f => r) -> (Constraints l p f => r)

-- | Every flavour of optic can be used as itself.
instance Is k k where
  implies _ = id

-- | Proxy type for use as an argument to 'implies'.
--
data IsProxy (k :: *) (l :: *) (p :: * -> * -> *) (f :: * -> *) =
  IsProxy

-- | Explicit cast from one optic flavour to another.
--
-- This is the identity function, modulo some constraint jiggery-pokery.
--
sub :: forall k l s t a b . Is k l => Optic k s t a b -> Optic l s t a b
sub (Optic o) = Optic (implies' o)
  where
    implies' :: forall p f . Optic__ k p f s t a b -> Optic__ l p f s t a b
    implies' x = implies (IsProxy :: IsProxy k l p f) x

-- | Computes the least upper bound of two optics flavours.
--
-- An instance of @Join k l m@ represents that the least upper bound
-- of an @Optic k@ and an @Optic l@ is an @Optic m@. This means in
-- particular that composition of an @Optic k@ and an @Optic k@ will
-- yield an @Optic m@.
--
class (Is k m, Is l m) => Join k l m | k l -> m

-- | Every optics flavour can be joined with itself.
instance Join k k k

-- | Compose two optics of compatible flavours.
--
-- Returns an optic of the appropriate supertype.
--
(%) :: Join k l m => Optic k s t u v -> Optic l u v a b -> Optic m s t a b
o % o' = sub o %% sub o'

-- | Compose two optics of the same flavour.
(%%) :: Optic k s t u v -> Optic k u v a b -> Optic k s t a b
Optic o %% Optic o' = Optic (o . o')
