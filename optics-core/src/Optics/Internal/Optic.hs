{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Core optic types and subtyping machinery.
--
-- This module contains the core 'Optic' types, and the underlying
-- machinery that we need in order to implement the subtyping between
-- various different flavours of optics.
--
-- The composition operator for optics is also defined here.
--
-- This module is intended for internal use only, and may change without
-- warning in subsequent releases.
--
module Optics.Internal.Optic
  ( Optic(..)
  , Optic'
  , Optic_
  , Optic__
  , getOptic
  , castOptic
  , (%)
  , (%%)
  , (%&)
  -- * Re-exports
  , module Optics.Internal.Optic.Subtyping
  , module Optics.Internal.Optic.Types
  , module Optics.Internal.Optic.TypeLevel
  ) where

import Data.Function ((&))
import Data.Type.Equality

import Data.Profunctor.Indexed

import Optics.Internal.Optic.Subtyping
import Optics.Internal.Optic.TypeLevel
import Optics.Internal.Optic.Types

-- | Wrapper newtype for the whole family of optics.
--
-- The first parameter @k@ identifies the particular optic kind (e.g. 'A_Lens'
-- or 'A_Traversal').
--
-- The parameter @is@ is a list of types available as indices.  This will
-- typically be 'NoIx' for unindexed optics, or 'WithIx' for optics with a
-- single index. See the "Indexed optics" section of the overview documentation
-- in the @Optics@ module of the main @optics@ package for more details.
--
-- The parameters @s@ and @t@ represent the "big" structure,
-- whereas @a@ and @b@ represent the "small" structure.
--
newtype Optic (k :: OpticKind) (is :: IxList) s t a b
  = Optic (forall p i. Profunctor p => Optic_ k p i (Curry is i) s t a b)

-- | Strip the newtype wrapper off.
getOptic
  :: Profunctor p
  => Optic k is s t a b
  -> Optic_ k p i (Curry is i) s t a b
-- Note: This is not part of the definition of 'Optic' because it needs to be
-- marked INLINE for GHC to optimize away profunctor classes when profiling.
-- See https://github.com/well-typed/optics/issues/324 for more details.
getOptic (Optic o) = o
{-# INLINE getOptic #-}

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
type Optic_ k p i j s t a b = Constraints k p => Optic__ p i j s t a b

-- | Optic internally as a profunctor transformation.
type Optic__ p i j s t a b = p i a b -> p j s t

-- | Explicit cast from one optic flavour to another.
--
-- The resulting optic kind is given in the first type argument, so you can use
-- TypeApplications to set it. For example
--
-- @
--  'castOptic' @'A_Lens' o
-- @
--
-- turns @o@ into a 'Optics.Lens.Lens'.
--
-- This is the identity function, modulo some constraint jiggery-pokery.
--
castOptic
  :: forall destKind srcKind is s t a b
  .  Is srcKind destKind
  => Optic srcKind  is s t a b
  -> Optic destKind is s t a b
castOptic (Optic o) = Optic (cast o)
  where
    cast
      :: forall p i
      .  Optic_ srcKind  p i (Curry is i) s t a b
      -> Optic_ destKind p i (Curry is i) s t a b
    cast x = implies @srcKind @destKind @p x

-- | Compose two optics of compatible flavours.
--
-- Returns an optic of the appropriate supertype.  If either or both optics are
-- indexed, the composition preserves all the indices.
--
infixl 9 %
(%) :: (Is k m, Is l m, m ~ Join k l, ks ~ Append is js)
    => Optic k is s t u v
    -> Optic l js u v a b
    -> Optic m ks s t a b
o % o' = castOptic o %% castOptic o'
{-# INLINE (%) #-}

-- | Compose two optics of the same flavour.
--
-- Normally you can simply use ('%') instead, but this may be useful to help
-- type inference if the type of one of the optics is otherwise
-- under-constrained.
infixl 9 %%
(%%) :: forall k is js ks s t u v a b. ks ~ Append is js
     => Optic k is s t u v
     -> Optic k js u v a b
     -> Optic k ks s t a b
Optic o %% Optic o' = Optic oo
  where
    oo :: forall p i. (Profunctor p, Constraints k p) => Optic__ p i (Curry ks i) s t a b
    oo | Refl <- appendIndices @is @js @i = o . o'

-- | Flipped function application, specialised to optics and binding tightly.
--
-- Useful for post-composing optics transformations:
--
-- >>> toListOf (ifolded %& ifiltered (\i s -> length s <= i)) ["", "a","abc"]
-- ["","a"]
--
infixl 9 %&
(%&) :: Optic k is s t a b
     -> (Optic k is s t a b -> Optic l js s' t' a' b')
     -> Optic l js s' t' a' b'
(%&) = (&)

-- $setup
-- >>> import Optics.Core
