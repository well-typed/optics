-- |
-- Module: GHC.Generics.Optics
-- Description: Optics for types defined in "GHC.Generics".
--
-- /Note:/ "GHC.Generics" exports a number of names that collide with "Optics"
-- (at least 'GHC.Generics.to').
--
-- You can use hiding of imports to mitigate this to an extent. The following
-- imports represent a fair compromise for user code:
--
-- @
-- import "Optics"
-- import "GHC.Generics" hiding (to)
-- import "GHC.Generics.Optics"
-- @
--
-- You can use 'generic' to replace 'GHC.Generics.from' and 'GHC.Generics.to'
-- from "GHC.Generics".
--
module GHC.Generics.Optics
  ( generic
  , generic1
  , _V1
  , _U1
  , _Par1
  , _Rec1
  , _K1
  , _M1
  , _L1
  , _R1
  ) where

import Optics.Internal.Generic
