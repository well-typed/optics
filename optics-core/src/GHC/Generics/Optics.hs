{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Generics.Lens
-- Copyright   :  (C) 2012-16 Edward Kmett, (C) 2018 Well-Typed LLP
--
-- Note: "GHC.Generics" exports a number of names that collide with "Optics" (at least 'to').
--
-- You can use hiding or imports to mitigate this to an extent, and the following imports,
-- represent a fair compromise for user code:
--
-- @
-- import "Optics"
-- import "GHC.Generics" hiding (to)
-- @
--
-- You can use 'generic' to replace 'GHC.Generics.from' and 'GHC.Generics.to' from "GHC.Generics".
--
module GHC.Generics.Optics
  (
    generic
  , generic1
  , _V1
  , _U1
  , _Par1
  , _Rec1
  , _K1
  , _M1
  , _L1
  , _R1
  , _1
  , _2
  ) where

import qualified GHC.Generics as GHC (to, from, to1, from1)
import GHC.Generics (Generic, Rep, Generic1, Rep1, (:+:) (..), (:*:) (..), V1, U1 (..), K1 (..), M1 (..), Par1 (..), Rec1 (..))

import Optics hiding (_1, _2) -- TODO: :*: fields
import qualified Data.Tuple.Optics as Tuple

-- | Convert from the data type to its representation (or back)
--
-- >>> view (generic % re generic) "hello" :: String
-- "hello"
--
generic :: Generic a => Iso' i a (Rep a b)
generic = iso GHC.from GHC.to
{-# INLINE generic #-}

-- | Convert from the data type to its representation (or back)
generic1 :: Generic1 f => Iso' i (f a) (Rep1 f a)
generic1 = iso GHC.from1 GHC.to1
{-# INLINE generic1 #-}

_V1 :: Lens i (V1 s) (V1 t) a b
_V1 = lens absurd absurd where
  absurd !_a = undefined
{-# INLINE _V1 #-}

_U1 :: Iso i (U1 p) (U1 q) () ()
_U1 = iso (const ()) (const U1)
{-# INLINE _U1 #-}

_Par1 :: Iso i (Par1 p) (Par1 q) p q
_Par1 = iso unPar1 Par1
{-# INLINE _Par1 #-}

_Rec1 :: Iso i (Rec1 f p) (Rec1 g q) (f p) (g q)
_Rec1 = iso unRec1 Rec1
{-# INLINE _Rec1 #-}

_K1 :: Iso i (K1 i c p) (K1 j d q) c d
_K1 = iso unK1 K1
{-# INLINE _K1 #-}

_M1 :: Iso i (M1 i c f p) (M1 j d g q) (f p) (g q)
_M1 = iso unM1 M1
{-# INLINE _M1 #-}

-- TODO: Add Field1 Field2 instances for (f :*: g)

-- not exported
_Pair :: Iso' i ((f :*: g) a) (f a, g a)
_Pair = iso (\(x :*: y) -> (x, y)) (\(x, y) -> x :*: y)

_1 :: Lens' i ((f :*: g) a) (f a)
_1 = _Pair % Tuple._1

_2 :: Lens' i ((f :*: g) a) (g a)
_2 = _Pair % Tuple._2

_L1 :: Prism' i ((f :+: g) a) (f a)
_L1 = prism L1 reviewer
  where
  reviewer (L1 l) = Right l
  reviewer x = Left x
{-# INLINE _L1 #-}

_R1 :: Prism' i ((f :+: g) a) (g a)
_R1 = prism R1 reviewer
  where
  reviewer (R1 l) = Right l
  reviewer x = Left x
{-# INLINE _R1 #-}

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Optics
