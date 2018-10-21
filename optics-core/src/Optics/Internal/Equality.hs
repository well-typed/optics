{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Optics.Internal.Equality where

import Optics.Internal.Optic

-- | Tag for an equality.
data An_Equality

-- | Constraints corresponding to an equality.
type instance Constraints An_Equality p = ()

-- | Type synonym for a type-modifying equality.
type Equality s t a b = Optic An_Equality s t a b

-- | Type synonym for a type-preserving equality.
type Equality' s a = Optic' An_Equality s a

-- | Explicitly cast an optic to an iso.
toEquality :: Is k An_Equality => Optic k s t a b -> Equality s t a b
toEquality = sub
{-# INLINE toEquality #-}

-- | Build an equality from the van Laarhoven representation.
vlEquality :: (forall p f . p a (f b) -> p s (f t)) -> Equality s t a b
vlEquality f = case f Identical of Identical -> Optic id
{-# INLINE vlEquality #-}

-- | Capture type constraints as an equality.
equality :: (s ~ a, t ~ b) => Equality s t a b
equality = Optic id
{-# INLINE equality #-}

-- | Proof of reflexivity.
simple :: Equality' a a
simple = Optic id
{-# INLINE simple #-}

-- | Substituting types with 'Equality'.
substEq :: Equality s t a b -> ((s ~ a, t ~ b) => r) -> r
substEq l = case runEq l of
  Identical -> \r -> r
{-# INLINE substEq #-}

----------------------------------------

-- | Provides a witness of equality.
data Identical (s :: *) (t :: *) (a :: *) (b :: *) where
  Identical :: (s ~ a, t ~ b) => Identical s t a b

-- | Obtain a witness for an equality.
runEq :: Is k An_Equality => Optic k s t a b -> Identical s t a b
runEq o =
  case (getOptic (toEquality o)) Identical of
    Identical -> Identical
{-# INLINE runEq #-}
