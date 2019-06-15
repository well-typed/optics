{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Optics.Generic.Product.Fields
  ( -- * Lenses
    field
  , field'
  , GL.HasField
  , GL.HasField'
  ) where

import qualified Data.Generics.Product.Fields as GL
import           Optics.Core

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGADTs
-- >>> :set -XFlexibleContexts
-- >>> import GHC.Generics
-- >>> import Optics.Core
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


-- | A 'Lens' that focuses on a field with a given name.
--
-- >>> view (field @"age") human
-- 50
--
-- === /Type changing/
--
-- >>> :t human
-- human :: Human Bool
--
-- >>> :t set (field @"other") (42 :: Int) human
-- set (field @"other") (42 :: Int) human :: Human Int
--
-- >>> set (field @"other") (42 :: Int) human
-- Human {name = "Tunyasz", age = 50, address = "London", other = 42}
--
field :: forall field s t a b. GL.HasField field s t a b => Lens s t a b
field = lensVL (GL.field @field)

field' :: forall field s a. GL.HasField' field s a => Lens' s a
field' = lensVL (GL.field' @field)
