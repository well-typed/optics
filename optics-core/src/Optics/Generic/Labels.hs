{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
-- | Provides 'IsLabel' instances for field/position lenses and prisms.
--
-- >>> :set -XOverloadedLabels
--
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> :{
-- data Human a
--   = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     , other   :: a
--     }
--   | HumanNoAddress
--     { name    :: String
--     , age     :: Int
--     , other   :: a
--     }
--   deriving (Generic, Show)
-- human :: Human Bool
-- human = Human { name = "Tunyasz", age = 50, address = "London", other = False }
-- :}
--
-- === Focus on a field
--
-- >>> view #age human
-- 50
--
-- === /Type changing/
--
-- >>> :t human
-- human :: Human Bool
--
-- >>> :t set #other (42 :: Int) human
-- set #other (42 :: Int) human :: Human Int
--
-- >>> set #other (42 :: Int) human
-- Human {name = "Tunyasz", age = 50, address = "London", other = 42}
--
-- === Focus on a position
--
-- >>> view #_1 human
-- "Tunyasz"
--
-- >>> over #_2 (+1) human
-- Human {name = "Tunyasz", age = 51, address = "London", other = False}
--
-- === Match on a constructor
--
-- >>> :t preview #_Human human
-- preview #_Human human :: Maybe ([Char], Int, [Char], Bool)
--
-- >>> preview #_Human human
-- Just ("Tunyasz",50,"London",False)
--
-- >>> preview #_HumanNoAddress human
-- Nothing
--
module Optics.Generic.Labels () where

import Data.Type.Bool
import Data.Type.Equality
import GHC.OverloadedLabels
import GHC.TypeLits

import Optics.Generic.Product.Fields
import Optics.Generic.Product.Positions
import Optics.Lens

#if __GLASGOW_HASKELL__ >= 802
import Optics.Generic.Sum.Constructors
import Optics.Prism
#endif

instance
  (method ~ Method name, MethodSelector method k s t a b, is ~ NoIx
  ) => IsLabel name (Optic k is s t a b) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = methodSelector @method @k @s @t @a @b
#else
  fromLabel _ = methodSelector @method @k @s @t @a @b
#endif

----------------------------------------

data MPosition    (pos  :: Nat)
data MField       (name :: Symbol)
data MConstructor (ctor :: Symbol)

type family Method (name :: Symbol) where
  Method "_1" = MPosition 1
  Method "_2" = MPosition 2
  Method "_3" = MPosition 3
  Method "_4" = MPosition 4
  Method "_5" = MPosition 5
  Method "_6" = MPosition 6
  Method "_7" = MPosition 7
  Method "_8" = MPosition 8
  Method "_9" = MPosition 9
  Method name = If
    (CmpSymbol "_@" name == 'LT && CmpSymbol "_[" name == 'GT)
    (MConstructor name)
    (MField name)

----------------------------------------

-- Below we mark the instances for type-changing optics (i.e. the most generic
-- ones) as INCOHERENT and make use of the following instance resolution rule:
--
-- "If exactly one non-incoherent candidate remains, select it. If all remaining
-- candidates are incoherent, select an arbitrary one. Otherwise the search
-- fails (i.e. when more than one surviving candidate is not incoherent)."
--
-- (see
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-IncoherentInstances
-- for more information)
--
-- to be able to have deterministic (because after we match on a method, we have
-- one non-incoherent and one incoherent instance to choose from, so if we deal
-- with type-preserving optic, the non-incoherent will fail to match and we can
-- select the incoherent one unambigously) choice between generation of
-- type-preserving and type-modifying optics as type-preserving variants should
-- compile faster.

class MethodSelector method k s t a b where
  methodSelector :: Optic k NoIx s t a b

instance {-# INCOHERENT #-}
  (HasPosition pos s t a b, k ~ A_Lens
  ) => MethodSelector (MPosition pos) k s t a b where
  methodSelector = position @pos @s @t @a @b

instance
  (HasPosition' pos s a, k ~ A_Lens, s ~ t
  ) => MethodSelector (MPosition pos) k s t a a where
  methodSelector = position' @pos @s @a

instance {-# INCOHERENT #-}
  (HasField name s t a b, k ~ A_Lens
  ) => MethodSelector (MField name) k s t a b where
  methodSelector = field @name @s @t @a @b

instance
  (HasField' name s a, k ~ A_Lens, s ~ t
  ) => MethodSelector (MField name) k s t a a where
  methodSelector = field' @name @s @a

#if __GLASGOW_HASKELL__ >= 802

instance {-# INCOHERENT #-}
  (_ctor ~ AppendSymbol "_" ctor, AsConstructor ctor s t a b, k ~ A_Prism
  ) => MethodSelector (MConstructor _ctor) k s t a b where
  methodSelector = _Ctor @ctor @s @t @a @b

instance
  (_ctor ~ AppendSymbol "_" ctor, AsConstructor' ctor s a, k ~ A_Prism, s ~ t
  ) => MethodSelector (MConstructor _ctor) k s t a a where
  methodSelector = _Ctor' @ctor @s @a

#else

instance
  (TypeError ('Text "Labels as prisms require at least GHC 8.2")
  ) => MethodSelector (MConstructor _ctor) k s t a b where
  methodSelector = error "unreachable"

#endif
