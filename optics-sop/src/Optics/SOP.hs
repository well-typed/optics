{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Optics.SOP
  ( WrappedLens(..)
  , WrappedNPPrism(..)
  , WrappedPrism(..)
  , lenses
  , npPrisms
  , prisms
  , LensesFor(..)
  , NPPrismsFor(..)
  , PrismsFor(..)
  , mkLenses
  , mkNPPrisms
  , mkPrisms
  )
  where

import Data.Coerce
import Generics.SOP
import Optics

import Generics.SOP.Optics
import Optics.SOP.ToTuple

-- | Wrapped form of a simple lens, to enable partial application.
newtype WrappedLens    i s a  = WrappedLens    { unWrapLens :: Lens' i s a }
-- | Wrapped form of a prism to an 'NP', to enable partial application.
newtype WrappedNPPrism i s xs = WrappedNPPrism { unWrapNPPrism :: Prism' i s (NP I xs) }
-- | Wrapped form of a prism to a tuple, to enable partial application.
newtype WrappedPrism   i s xs = WrappedPrism   { unWrapPrism :: Prism' i s (ToTuple xs) }

-- | Produce an 'NP' of wrapped lenses for a generic product type.
lenses ::
  forall i a xs .
  (IsProductType a xs) => NP (WrappedLens i a) xs
lenses = hmap (coerce (productRep %)) (go sList)
  where
    go :: forall ys . SList ys -> NP (WrappedLens i (NP I ys)) ys
    go SNil  = Nil
    go SCons = coerce (npHead % i) :* hmap (coerce (npTail %)) (go sList)

-- | Produce an 'NP' of wrapped prisms for a generic product type.
--
-- Prisms match the big type to a product of the constructor arguments.
-- In this variant, 'NP' is used to represent these products.
--
npPrisms ::
  forall i a xss .
  (Generic a, Code a ~ xss) => NP (WrappedNPPrism i a) xss
npPrisms = hmap (coerce (rep % sop %)) (go sList)
  where
    go :: forall yss . SList yss -> NP (WrappedNPPrism i (NS (NP I) yss)) yss
    go SNil  = Nil
    go SCons = coerce _Z :* hmap (coerce (_S %)) (go sList)

-- | Produce an 'NP' of wrapped prisms for a generic product type.
--
-- Prisms match the big type to a product of the constructor arguments.
-- In this variant, tuples are used to represent these products.
--
prisms ::
  forall i a xss .
  (Generic a, Code a ~ xss, All TupleLike xss) => NP (WrappedPrism i a) xss
prisms =
  hcmap (Proxy :: Proxy TupleLike)
    (\ (WrappedNPPrism p) -> WrappedPrism (p % tuple)) npPrisms

-- | Map a type-level function over a type-level list.
--
-- Used to unpack the wrapped variants of optics in a product.
--
type family Unpack f xs where
  Unpack f '[]       = '[]
  Unpack f (x ': xs) = ApplyWrapped f x ': Unpack f xs

-- | Type-level function that unwraps a wrapped optic.
type family ApplyWrapped (f :: k -> *) (x :: k) :: *
type instance ApplyWrapped (WrappedLens i s)    a  = Lens' i s a
type instance ApplyWrapped (WrappedNPPrism i s) xs = Prism' i s (NP I xs)
type instance ApplyWrapped (WrappedPrism i s)   xs = Prism' i s (ToTuple xs)

-- | Convenience type synonym combining unpacking a type-level
-- list with turning it into a tuple.
--
type TupleUnpack f xs = (TupleLike (Unpack f xs)) => ToTuple (Unpack f xs)

-- | Given an unpacking function for the elements,
-- unpack an 'NP' and turn it into a tuple.
--
tupleUnpack ::
  forall f xs .
  (forall x . f x -> ApplyWrapped f x) -> NP f xs -> TupleUnpack f xs
tupleUnpack un = view tuple . go
  where
    go :: forall ys . NP f ys -> NP I (Unpack f ys)
    go Nil       = Nil
    go (x :* xs) = I (un x) :* go xs

-- | Wrapper type holding the lenses for a generic datatype as a tuple.
newtype LensesFor i a =
  Lenses ((Generic a) => TupleUnpack (WrappedLens i a) (Extract (Code a)))

-- | Helper function needed by 'LensesFor'.
type family Extract xss where
  Extract '[ xs ] = xs

-- | Wrapper type holding the ('NP'-)prisms for a generic datatype as a tuple.
data NPPrismsFor i a =
  NPPrisms ((Generic a) => TupleUnpack (WrappedNPPrism i a) (Code a))

-- | Wrapper type holding the (tuple-)prisms for a generic datatype as a tuple.
data PrismsFor i a =
  Prisms ((Generic a) => TupleUnpack (WrappedPrism i a) (Code a))

-- | Produce all lenses for a generic record datatype.
mkLenses :: forall i a xs . (IsProductType a xs) => LensesFor i a
mkLenses =
  Lenses (tupleUnpack unWrapLens (lenses :: NP (WrappedLens i a) xs))

-- | Produce all ('NP'-)prisms for a generic datatype.
mkNPPrisms :: forall i a xss . (Generic a, Code a ~ xss) => NPPrismsFor i a
mkNPPrisms =
  NPPrisms (tupleUnpack unWrapNPPrism (npPrisms :: NP (WrappedNPPrism i a) xss))

-- | Produce all (tuple-)prisms for a generic datatype.
mkPrisms :: forall i a xss . (Generic a, Code a ~ xss, All TupleLike xss) => PrismsFor i a
mkPrisms =
 Prisms (tupleUnpack unWrapPrism (prisms :: NP (WrappedPrism i a) xss))
