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
--
-- @since 0.2
type IxList = [Type]

-- | An alias for an empty index-list
type NoIx = ('[] :: IxList)

-- | Singleton index list
type WithIx i = ('[i] :: IxList)

-- | Show a type surrounded by quote marks.
type family QuoteType (x :: Type) :: ErrorMessage where
  QuoteType x = 'Text "‘" ':<>: 'ShowType x ':<>: 'Text "’"

-- | Show a symbol surrounded by quote marks.
type family QuoteSymbol (x :: Symbol) :: ErrorMessage where
  QuoteSymbol x = 'Text "‘" ':<>: 'Text x ':<>: 'Text "’"

----------------------------------------
-- Elimination forms in error messages

type family ShowSymbolWithOrigin symbol origin :: ErrorMessage where
  ShowSymbolWithOrigin symbol origin = 'Text "  "
                                 ':<>: QuoteSymbol symbol
                                 ':<>: 'Text " (from "
                                 ':<>: 'Text origin
                                 ':<>: 'Text ")"

type family ShowSymbolsWithOrigin (fs :: [(Symbol, Symbol)]) :: ErrorMessage where
  ShowSymbolsWithOrigin '[ '(symbol, origin) ] =
    ShowSymbolWithOrigin symbol origin
  ShowSymbolsWithOrigin ('(symbol, origin) ': rest) =
    ShowSymbolWithOrigin symbol origin ':$$: ShowSymbolsWithOrigin rest

type family ShowOperators (ops :: [Symbol]) :: ErrorMessage where
  ShowOperators '[op] =
    QuoteSymbol op ':<>: 'Text " (from Optics.Operators)"
  ShowOperators (op ': rest) =
    QuoteSymbol op ':<>: 'Text " " ':<>: ShowOperators rest

type family AppendEliminations a b where
  AppendEliminations '(fs1, ops1) '(fs2, ops2) =
    '(Append fs1 fs2, Append ops1 ops2)

type family ShowEliminations forms :: ErrorMessage where
  ShowEliminations '(fs, ops) =
    ShowSymbolsWithOrigin fs ':$$: 'Text "  " ':<>: ShowOperators ops

----------------------------------------

data RepDefined = RepDefined
-- | This type family should be called with applications of 'Rep' on both sides,
-- and will reduce to 'RepDefined' if at least one of them is defined; otherwise
-- it is stuck.
type family AnyHasRep (s :: Type -> Type) (t :: Type -> Type) :: RepDefined
type instance AnyHasRep (s x) t = 'RepDefined
type instance AnyHasRep s (t x) = 'RepDefined

-- | Curry a type-level list.
--
-- In pseudo (dependent-)Haskell:
--
-- @
-- 'Curry' xs y = 'foldr' (->) y xs
-- @
type family Curry (xs :: IxList) (y :: Type) :: Type where
  Curry '[]       y = y
  Curry (x ': xs) y = x -> Curry xs y

-- | Append two type-level lists together.
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
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
