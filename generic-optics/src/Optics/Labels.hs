{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
-- | Provides (an orphan) 'IsLabel' instances for field lenses (/TODO/ constructor prisms).
--
-- This module is meant to be imported only for @'IsLabel' name 'Optic'@ instance.
--
-- @
-- import Optics.Labels ()
-- @
--
-- >>> :set -XOverloadedLabels
--
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
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
module Optics.Labels where

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (Symbol)
import Optics
import Optics.Generic.Product.Fields

class Field (name :: Symbol) s t a b | s name -> a, t name -> b, s name b -> t, t name a -> s where
  fieldLens :: Lens s t a b

instance {-# INCOHERENT #-} HasField name s t a b => Field name s t a b where
  fieldLens = field @name

instance {-# INCOHERENT #-} HasField' name s a => Field name s s a a where
  fieldLens = field' @name

-- we have simple instance for IsLabel, as we don't support prisms yet

instance (Field name s t a b, is ~ '[], k ~ A_Lens) => IsLabel name (Optic k is s t a b) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = fieldLens @name @s @t @a @b
#else
  fromLabel _ = fieldLens @name @s @t @a @b
#endif
