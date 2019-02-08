{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal.Generic.Sum.Constructors
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive constructor-name-based prisms generically.
--
-----------------------------------------------------------------------------

module Optics.Internal.Generic.Sum.Constructors
  ( GAsConstructor (..)
  , GAsConstructor'
  ) where


import GHC.Generics
import GHC.Generics.Optics
import GHC.TypeLits (Symbol)

import Optics.Internal.Generic.Families
import Optics.Internal.Generic.Product.HList
import Optics.Iso
import Optics.Prism

-- |As 'AsConstructor' but over generic representations as defined by
--  "GHC.Generics".
class GAsConstructor (ctor :: Symbol) s t a b | ctor s -> a, ctor t -> b where
  _GCtor :: Prism (s x) (t x) a b

type GAsConstructor' ctor s a = GAsConstructor ctor s s a a

instance
  ( GIsList f g as bs
  , ListTuple a as
  , ListTuple b bs
  ) => GAsConstructor ctor (M1 C ('MetaCons ctor fixity fields) f) (M1 C ('MetaCons ctor fixity fields) g) a b where

  _GCtor = toPrism $ _M1 % glist % tupled
  {-# INLINE _GCtor #-}

instance GSumAsConstructor ctor (HasCtorP ctor l) l r l' r' a b => GAsConstructor ctor (l :+: r) (l' :+: r') a b where
  _GCtor = _GSumCtor @ctor @(HasCtorP ctor l)
  {-# INLINE _GCtor #-}

instance GAsConstructor ctor f f' a b => GAsConstructor ctor (M1 D meta f) (M1 D meta f') a b where
  _GCtor = _M1 % _GCtor @ctor
  {-# INLINE _GCtor #-}

class GSumAsConstructor (ctor :: Symbol) (contains :: Bool) l r l' r' a b | ctor l r -> a, ctor l' r' -> b where
  _GSumCtor :: Prism ((l :+: r) x) ((l' :+: r') x) a b

instance GAsConstructor ctor l l' a b => GSumAsConstructor ctor 'True l r l' r a b where
  _GSumCtor = _L1 % _GCtor @ctor
  {-# INLINE _GSumCtor #-}

instance GAsConstructor ctor r r' a b => GSumAsConstructor ctor 'False l r l r' a b where
  _GSumCtor = _R1 % _GCtor @ctor
  {-# INLINE _GSumCtor #-}
