module Optics.Internal.Equality where

import Optics.Internal.Optic

-- | Type synonym for a type-modifying equality.
type Equality i s t a b = Optic An_Equality i i s t a b

-- | Type synonym for a type-preserving equality.
type Equality' i s a = Optic' An_Equality i i s a

-- | Build an equality from the van Laarhoven representation.
vlEquality :: (forall p f . p a (f b) -> p s (f t)) -> Equality i s t a b
vlEquality f = case f Identical of Identical -> Optic id
{-# INLINE vlEquality #-}

-- | Capture type constraints as an equality.
equality :: (s ~ a, t ~ b) => Equality i s t a b
equality = Optic id
{-# INLINE equality #-}

-- | Proof of reflexivity.
simple :: Equality' i a a
simple = Optic id
{-# INLINE simple #-}

-- | Substituting types with 'Equality'.
withEq :: Equality i s t a b -> ((s ~ a, t ~ b) => r) -> r
withEq l = case runEq l of Identical -> \r -> r
{-# INLINE withEq #-}

----------------------------------------

-- | Provides a witness of equality.
data Identical (s :: *) (t :: *) (a :: *) (b :: *) where
  Identical :: (s ~ a, t ~ b) => Identical s t a b

-- | Helper for stripping unused index type variable.
newtype IxIdentical s t i a b =
  IxIdentical { unIxIdentical :: Identical s t a b }

-- | Obtain a witness for an equality.
runEq :: Equality i s t a b -> Identical s t a b
runEq o =
  case unIxIdentical $ getOptic o (IxIdentical Identical) of
    Identical -> Identical
{-# INLINE runEq #-}
