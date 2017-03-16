{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Equality where

import Optics.Internal.Optic

-- | Tag for an equality.
data An_Equality

-- | Constraints corresponding to an equality.
type instance Constraints An_Equality p f = ()

-- | Type synonym for a type-modifying equality.
type Equality s t a b = Optic An_Equality s t a b

-- | Type synonym for a type-preserving equality.
type Equality' s a = Optic' An_Equality s a

-- | Explicitly cast an optic to an iso.
toEquality :: Is k An_Equality => Optic k s t a b -> Equality s t a b
toEquality = sub
{-# INLINE toEquality #-}

-- | Create an equality.
mkEquality :: Optic_ An_Equality s t a b -> Equality s t a b
mkEquality = Optic
{-# INLINE mkEquality #-}

-- | Proof of reflexivity.
simple :: Equality' a a
simple = Optic id
{-# INLINE simple #-}

-- | Provides a witness of equality.
data Identical (s :: *) (t :: *) (a :: *) (b :: *) where
  Identical :: (s ~ a, t ~ b) => Identical s t a b

-- | Obtain a witness for an equality.
runEq :: Is k An_Equality => Optic k s t a b -> Identical s t a b
runEq o =
  case (getOptic (toEquality o)) Identical of
    Identical -> Identical
{-# INLINE runEq #-}
