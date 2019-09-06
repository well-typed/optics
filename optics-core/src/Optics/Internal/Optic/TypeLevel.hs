{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Optic.TypeLevel where

import Data.Kind (Type)
import GHC.TypeLits

-- | A list of index types, used for indexed optics.
type Indices = [Type]

-- | An alias for an empty index-list
type NoIx = ('[] :: Indices)

-- | Singleton index list
type WithIx i = ('[i] :: Indices)

-- | Show a type surrounded by quote marks.
type family QuoteType (x :: Type) :: ErrorMessage where
  QuoteType x = 'Text "‘" ':<>: 'ShowType x ':<>: 'Text "’"

-- | Curry a type-level list.
--
-- In pseudo (dependent-)Haskell:
--
-- @
-- 'Curry' xs y = 'foldr' (->) y xs
-- @
type family Curry (xs :: Indices) (y :: Type) :: Type where
  Curry '[]       y = y
  Curry (x ': xs) y = x -> Curry xs y

-- | Append two type-level lists together.
type family Append (xs :: Indices) (ys :: Indices) :: Indices where
  Append '[]       ys  = ys -- needed for (<%>) and (%>)
  Append xs        '[] = xs -- needed for (<%)
  Append (x ': xs) ys  = x ': Append xs ys

-- | Class that is inhabited by all type-level lists @xs@, providing the ability
-- to compose a function under @'Curry' xs@.
class CurryCompose xs where
  -- | Compose a function under @'Curry' xs@.  This generalises @('.')@ (aka
  -- 'fmap' for @(->)@) to work for curried functions with one argument for each
  -- type in the list.
  composeN :: (i -> j) -> Curry xs i -> Curry xs j

instance CurryCompose '[] where
  composeN = id
  {-# INLINE composeN #-}

instance CurryCompose xs => CurryCompose (x ': xs) where
  composeN ij f = composeN @xs ij . f
  {-# INLINE composeN #-}
