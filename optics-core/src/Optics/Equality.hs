-- |
-- Module: Optics.Equality
-- Description: A proof of type equality.
--
-- An @'Equality' S T A B@ is a proof that @S@ is equal to @A@ and @T@ is equal
-- to @B@.
module Optics.Equality
  (
  -- * Formation
    Equality
  , Equality'

  -- * Introduction
  , equality
  , simple

  -- * Elimination
  -- | There are no optic kinds below 'Equality' in the subtyping poset, so the
  -- elimination forms are not polymorphic in the optic kind.
  , Identical(..)
  , runEquality

  -- * Computation
  -- |
  --
  -- @
  -- 'runEquality' 'equality' ≡ 'Identical'
  -- @

  -- * Additional elimination forms
  , withEquality

  -- * Subtyping
  , An_Equality

  -- * Re-exports
  , module Optics.Optic
  )
  where

import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for a type-modifying equality.
type Equality s t a b = Optic An_Equality NoIx s t a b

-- | Type synonym for a type-preserving equality.
type Equality' s a = Optic' An_Equality NoIx s a

-- | Capture type constraints as an equality.
--
-- /Note:/ This is the identity optic:
--
-- >>> :t view equality
-- view equality :: a -> a
equality :: (s ~ a, t ~ b) => Equality s t a b
equality = Optic id
{-# INLINE equality #-}

-- | Proof of reflexivity.
simple :: Equality' a a
simple = Optic id
{-# INLINE simple #-}

-- | Substituting types with 'Equality'.
withEquality :: Optic An_Equality is s t a b -> ((s ~ a, t ~ b) => r) -> r
withEquality l = case runEquality l of Identical -> \r -> r
{-# INLINE withEquality #-}

----------------------------------------

-- | Provides a witness of equality.
data Identical (s :: *) (t :: *) (a :: *) (b :: *) where
  Identical :: (s ~ a, t ~ b) => Identical s t a b

-- | Helper for stripping unused index type variable.
newtype IxIdentical s t i a b =
  IxIdentical { unIxIdentical :: Identical s t a b }

-- | Obtain a witness for an equality.
runEquality :: Optic An_Equality is s t a b -> Identical s t a b
runEquality o =
  case unIxIdentical $ getOptic o (IxIdentical Identical) of
    Identical -> Identical
{-# INLINE runEquality #-}
