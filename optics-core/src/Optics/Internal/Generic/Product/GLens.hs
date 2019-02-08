{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal.Generic.Product.GLens
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Optics.Internal.Generic.Product.GLens
  ( GLens (..)
  , GLens'
  , TyFun
  , Eval
  ) where

import Data.Kind    (Type)
import GHC.Generics
import GHC.Generics.Optics

import Optics.Internal.Generic.Utils
import Optics.Iso
import Optics.Lens

type Pred = TyFun (Type -> Type) (Maybe Type)

type TyFun a b = a -> b -> Type
type family Eval (f :: TyFun a b) (x :: a) :: b

-- A generic lens that uses some predicate to determine which field to focus on
class GLens (pred :: Pred) (s :: Type -> Type) (t :: Type -> Type) a b | s pred -> a, t pred -> b where
  glens :: Lens (s x) (t x) a b

type GLens' pred s a = GLens pred s s a a

instance GProductLens (Eval pred l) pred l r l' r' a b
      => GLens pred (l :*: r) (l' :*: r') a b where

  glens = gproductLens @(Eval pred l) @pred
  {-# INLINE glens #-}

instance (GLens pred l l' a b, GLens pred r r' a b) =>  GLens pred (l :+: r) (l' :+: r') a b where
  glens = gchoosing (glens @pred) (glens @pred)
  {-# INLINE glens #-}

instance GLens pred (K1 r a) (K1 r b) a b where
  glens = toLens _K1
  {-# INLINE glens #-}

instance (GLens pred f g a b) => GLens pred (M1 m meta f) (M1 m meta g) a b where
  glens = _M1 % glens @pred
  {-# INLINE glens #-}

class GProductLens (left :: Maybe Type) (pred :: Pred) l r l' r' a b | pred l r -> a, pred l' r' -> b where
  gproductLens :: Lens ((l :*: r) x) ((l' :*: r') x) a b

instance GLens pred l l' a b => GProductLens ('Just x) pred l r l' r a b where
  gproductLens = gfirst % glens @pred
  {-# INLINE gproductLens #-}

instance GLens pred r r' a b => GProductLens 'Nothing pred l r l r' a b where
  gproductLens = gsecond % glens @pred
  {-# INLINE gproductLens #-}
