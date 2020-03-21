{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Optics.Label.Generic
  () where

import Data.Generics.Product.Fields
import Data.Generics.Sum.Constructors
import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits

import Optics.Internal.Optic
import Optics.Lens
import Optics.Prism

type family GenericOpticKind (name :: Symbol) :: OpticKind where
  GenericOpticKind name =
    If (CmpSymbol "_@" name == 'LT && CmpSymbol "_[" name == 'GT)
      A_Prism
      A_Lens

instance
  ( k ~ GenericOpticKind name
  , GenericOptic name k s t a b
  ) => GeneralLabelOptic name k s t a b 'True where
  generalLabelOptic = genericOptic @name @k @s @t @a @b

----------------------------------------

class GenericOptic name k s t a b where
  genericOptic :: Optic k NoIx s t a b

instance Field name s t a b => GenericOptic name A_Lens s t a b where
  genericOptic = fieldLens @name @s @t @a @b

instance
  ( Constructor name s t a b
  , _name ~ AppendSymbol "_" name
  ) => GenericOptic _name A_Prism s t a b where
  genericOptic = constructorPrism @name @s @t @a @b

----------------------------------------

-- | 'Field' is morally the same as 'HasField', but it is constructed from an
-- incoherent combination of 'HasField' and 'HasField''. In this way, it can be
-- seamlessly used even when dealing with data types that don't have 'HasField'
-- instances (like data instances).
class Field name s t a b where
  fieldLens :: Lens s t a b

instance {-# INCOHERENT #-} HasField name s t a b => Field name s t a b where
  fieldLens = field @name @s @t @a @b

-- Use field' when appropriate for faster compile times. Note that if a ~ b,
-- then necessarily s ~ t. This doesn't apply in general because of phantom type
-- variables, but 'LabelOptic' doesn't support them.
instance (HasField' name s a, s ~ t) => Field name s t a a where
  fieldLens = field' @name @s @a

----------------------------------------

-- | 'Constructor' is morally the same as 'AsConstructor', but it is constructed
-- from an incoherent combination of 'AsConstructor' and 'AsConstructor''. In
-- this way, it can be seamlessly used even when dealing with data types that
-- don't have 'AsConstructor' instances (like data instances).
class Constructor name s t a b where
  constructorPrism :: Prism s t a b

instance {-# INCOHERENT #-} AsConstructor name s t a b => Constructor name s t a b where
  constructorPrism = _Ctor @name @s @t @a @b

-- Use Ctor' when appropriate for faster compile times. Note that if a ~ b, then
-- necessarily s ~ t. This doesn't apply in general because of phantom type
-- variables, but 'LabelOptic' doesn't support them.
instance (AsConstructor' name s a, s ~ t) => Constructor name s t a a where
  constructorPrism = _Ctor' @name @s @a
