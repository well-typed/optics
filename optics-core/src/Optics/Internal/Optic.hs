{-# LANGUAGE DataKinds #-}
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
  , NoIx
  , WithIx
  , castOptic
  , (%)
  , (%%)
  , IsProxy (..)
  , module Optics.Internal.Optic.Subtyping
  , module Optics.Internal.Optic.Types
  , module Optics.Internal.Optic.TypeLevel
  ) where

import Optics.Internal.Optic.Subtyping
import Optics.Internal.Optic.Types
import Optics.Internal.Optic.TypeLevel (Curry, Append)

-- to make %% simpler
import Unsafe.Coerce (unsafeCoerce)

-- | An alias for an empty index-list
type NoIx = '[]

-- | Singleton index list
type WithIx i = '[i]

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
newtype Optic (k :: *) (is :: [*]) s t a b =
  Optic { getOptic :: forall p j. Optic_ k p j (Curry is j) s t a b }

-- | Common special case of 'Optic' where source and target types are equal.
--
-- Here, we need only one "big" and one "small" type. For lenses, this
-- means that in the restricted form we cannot do type-changing updates.
--
type Optic' k is s a = Optic k is s s a a

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
data IsProxy (k :: *) (l :: *) (p :: * -> * -> * -> *) =
  IsProxy

-- | Explicit cast from one optic flavour to another.
--
-- This is the identity function, modulo some constraint jiggery-pokery.
--
-- TODO: add a graph
--
castOptic
  :: forall k l is s t a b
  .  Is k l
  => Optic k is s t a b
  -> Optic l is s t a b
castOptic (Optic o) = Optic (implies' o)
  where
    implies'
      :: forall p j
      .  Optic_ k p j (Curry is j) s t a b
      -> Optic_ l p j (Curry is j) s t a b
    implies' x = implies (IsProxy :: IsProxy k l p) x
{-# INLINE castOptic #-}

-- | Compose two optics of compatible flavours.
--
-- Returns an optic of the appropriate supertype.
--
infixr 9 %
(%) :: (Is k m, Is l m, m ~ Join k l, ks ~ Append is js)
    => Optic k is s t u v
    -> Optic l js u v a b
    -> Optic m ks s t a b
o % o' = castOptic o %% castOptic o'
{-# INLINE (%) #-}

-- | Compose two optics of the same flavour.
infixr 9 %%
(%%) :: forall k is js ks s t u v a b. ks ~ Append is js
     => Optic k is s t u v
     -> Optic k js u v a b
     -> Optic k ks s t a b
Optic o %% Optic o' = Optic oo
  where
    -- unsafeCoerce to the rescue
    oo :: forall p j. Optic_ k p j (Curry ks j) s t a b
    oo = (unsafeCoerce
           :: Optic_ k p j (Curry is (Curry js j)) s t a b
           -> Optic_ k p i (Curry ks j           ) s t a b)
      (o . o')
{-# INLINE (%%) #-}
