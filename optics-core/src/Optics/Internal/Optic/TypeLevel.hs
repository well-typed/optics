{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Optic.TypeLevel where

import Data.Kind
import GHC.TypeLits

-- | A list of index types, used for indexed optics.
--
-- @since 0.2
type IxList = [Type]

-- | An alias for an empty index-list
type NoIx = ('[] :: IxList)

-- | Singleton index list
type WithIx i = ('[i] :: IxList)

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
-- Lists

-- | Reverse a type-level list.
type family Reverse (xs :: [k]) (acc :: [k]) :: [k] where
  Reverse '[]      acc = acc
  Reverse (x : xs) acc = Reverse xs (x : acc)

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

instance CurryCompose xs => CurryCompose (x ': xs) where
  composeN ij f = composeN @xs ij . f

----------------------------------------
-- Indices

-- | Tagged version of 'Data.Type.Equality.(:~:)' for carrying evidence that two
-- index lists in a curried form are equal.
data IxEq i is js where
  IxEq :: IxEq i is is

-- | In pseudo (dependent-)Haskell, provide a witness
--
-- @
-- foldr f (foldr f init xs) ys = foldr f init (ys ++ xs)
--    where f = (->)
-- @
class AppendIndices xs ys where
  appendIndices :: IxEq i (Curry xs (Curry ys i)) (Curry (Append xs ys) i)

-- | If we know the second list is empty, we can pick the first list without
-- knowing anything about it, hence the instance is marked as INCOHERENT.
instance {-# INCOHERENT #-} AppendIndices xs '[] where
  appendIndices = IxEq

instance AppendIndices '[] ys where
  appendIndices = IxEq

instance
  (Append (x ': xs) ys ~ (x ': Append xs ys), AppendIndices xs ys
  ) => AppendIndices (x ': xs) ys where
  appendIndices :: forall i. IxEq i (Curry (x ': xs) (Curry ys i))
                                    (Curry (x ': Append xs ys) i)
  appendIndices | IxEq <- appendIndices @xs @ys @i = IxEq

----------------------------------------
-- Either

-- | If lhs is 'Right', return it. Otherwise check rhs.
type family FirstRight (m1 :: Either e a) (m2 :: Either e a) :: Either e a where
  FirstRight ('Right a) _ = 'Right a
  FirstRight          _ b = b

type family FromRight (def :: b) (e :: Either a b) :: b where
  FromRight _   ('Right b) = b
  FromRight def ('Left  _) = def

type family IsLeft (e :: Either a b) :: Bool where
  IsLeft ('Left _)  = 'True
  IsLeft ('Right _) = 'False

----------------------------------------
-- Errors

-- | Show a custom type error if @p@ is true.
type family When (p :: Bool) (err :: Constraint) :: Constraint where
  When 'True  err = err
  When 'False _   = ()

-- | Show a custom type error if @p@ is false (or stuck).
type family Unless (p :: Bool) (err :: Constraint) :: Constraint where
  Unless 'True  _   = ()
  Unless 'False err = err

-- | Use with 'Unless' to detect stuck (undefined) type families.
type family Defined (f :: k) :: Bool where
  Defined (f _) = Defined f
  Defined _     = 'True

-- | Show a type surrounded by quote marks.
type family QuoteType (x :: t) :: ErrorMessage where
  QuoteType x = 'Text "‘" ':<>: 'ShowType x ':<>: 'Text "’"

-- | Show a symbol surrounded by quote marks.
type family QuoteSymbol (x :: Symbol) :: ErrorMessage where
  QuoteSymbol x = 'Text "‘" ':<>: 'Text x ':<>: 'Text "’"

type family ToOrdinal (n :: Nat) :: ErrorMessage where
  ToOrdinal 1 = 'Text "1st"
  ToOrdinal 2 = 'Text "2nd"
  ToOrdinal 3 = 'Text "3rd"
  ToOrdinal n = 'ShowType n ':<>: 'Text "th"

----------------------------------------
-- Misc

-- | Derive the shape of @a@ from the shape of @b@.
class HasShapeOf (a :: k) (b :: k)
instance {-# OVERLAPPING #-} (fa ~ f a, HasShapeOf f g) => HasShapeOf fa (g b)
instance (a ~ b) => HasShapeOf a b
