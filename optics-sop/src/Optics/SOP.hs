{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
-- | This module provides machinery to construct lenses and prisms using
-- @generics-sop@.
--
-- If you have a datatype @T@ with a 'Generics.SOP.Generic' instance, you can
-- use @'mkLenses' :: 'LensesFor' T@ to obtain a tuple of lenses, or similarly
-- use @'mkPrisms' :: 'PrismsFor' T@ to obtain a tuple of prisms. For example:
--
-- >>> :set -XDeriveGeneric -XDeriveAnyClass -XStandaloneDeriving
-- >>> import qualified GHC.Generics as GHC
-- >>> import qualified Generics.SOP as SOP
-- >>> data T = MkT { _foo :: Int, _bar :: Bool } deriving GHC.Generic
-- >>> deriving instance SOP.Generic T
-- >>> Lenses (sopFoo, sopBar) = mkLenses :: LensesFor T
-- >>> view sopFoo (MkT 42 True)
-- 42
--
-- >>> Prisms (sopNothing, sopJust) = mkPrisms :: PrismsFor (Maybe a)
-- >>> isn't sopJust Nothing
-- True
--
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
import Data.Kind
import Generics.SOP
import Optics.Core

import Generics.SOP.Optics
import Optics.SOP.ToTuple

-- $setup
-- >>> import Optics.Core

-- | Wrapped form of a simple lens, to enable partial application.
newtype WrappedLens    s a  = WrappedLens    { unWrapLens :: Lens' s a }
-- | Wrapped form of a prism to an 'NP', to enable partial application.
newtype WrappedNPPrism s xs = WrappedNPPrism { unWrapNPPrism :: Prism' s (NP I xs) }
-- | Wrapped form of a prism to a tuple, to enable partial application.
newtype WrappedPrism   s xs = WrappedPrism   { unWrapPrism :: Prism' s (ToTuple xs) }

-- | Produce an 'NP' of wrapped lenses for a generic product type.
lenses ::
  forall a xs .
  (IsProductType a xs) => NP (WrappedLens a) xs
lenses = hmap (coerce (productRep %)) (go sList)
  where
    go :: forall ys . SList ys -> NP (WrappedLens (NP I ys)) ys
    go SNil  = Nil
    go SCons = coerce (npHead % _I) :* hmap (coerce (npTail %)) (go sList)

-- | Produce an 'NP' of wrapped prisms for a generic product type.
--
-- Prisms match the big type to a product of the constructor arguments.
-- In this variant, 'NP' is used to represent these products.
--
npPrisms ::
  forall a xss .
  (Generic a, Code a ~ xss) => NP (WrappedNPPrism a) xss
npPrisms = hmap (coerce ((rep % sop) %)) (go sList)
  where
    go :: forall yss . SList yss -> NP (WrappedNPPrism (NS (NP I) yss)) yss
    go SNil  = Nil
    go SCons = coerce _Z :* hmap (coerce (_S %)) (go sList)

-- | Produce an 'NP' of wrapped prisms for a generic product type.
--
-- Prisms match the big type to a product of the constructor arguments.
-- In this variant, tuples are used to represent these products.
--
prisms ::
  forall a xss .
  (Generic a, Code a ~ xss, All TupleLike xss) => NP (WrappedPrism a) xss
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
type family ApplyWrapped (f :: k -> Type) (x :: k) :: Type
type instance ApplyWrapped (WrappedLens s)    a  = Lens' s a
type instance ApplyWrapped (WrappedNPPrism s) xs = Prism' s (NP I xs)
type instance ApplyWrapped (WrappedPrism s)   xs = Prism' s (ToTuple xs)

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
tupleUnpack un xs0= view tuple (go xs0)
  where
    go :: forall ys . NP f ys -> NP I (Unpack f ys)
    go Nil       = Nil
    go (x :* xs) = I (un x) :* go xs

-- | Wrapper type holding the lenses for a generic datatype as a tuple.
newtype LensesFor a =
  Lenses ((Generic a) => TupleUnpack (WrappedLens a) (Extract (Code a)))

-- | Helper function needed by 'LensesFor'.
type family Extract xss where
  Extract '[ xs ] = xs

-- | Wrapper type holding the ('NP'-)prisms for a generic datatype as a tuple.
data NPPrismsFor a =
  NPPrisms ((Generic a) => TupleUnpack (WrappedNPPrism a) (Code a))

-- | Wrapper type holding the (tuple-)prisms for a generic datatype as a tuple.
data PrismsFor a =
  Prisms ((Generic a) => TupleUnpack (WrappedPrism a) (Code a))

-- | Produce all lenses for a generic record datatype.
mkLenses :: forall a xs . (IsProductType a xs) => LensesFor a
mkLenses =
  Lenses (tupleUnpack unWrapLens (lenses :: NP (WrappedLens a) xs))

-- | Produce all ('NP'-)prisms for a generic datatype.
mkNPPrisms :: forall a xss . (Generic a, Code a ~ xss) => NPPrismsFor a
mkNPPrisms =
  NPPrisms (tupleUnpack unWrapNPPrism (npPrisms :: NP (WrappedNPPrism a) xss))

-- | Produce all (tuple-)prisms for a generic datatype.
mkPrisms :: forall a xss . (Generic a, Code a ~ xss, All TupleLike xss) => PrismsFor a
mkPrisms =
 Prisms (tupleUnpack unWrapPrism (prisms :: NP (WrappedPrism a) xss))
