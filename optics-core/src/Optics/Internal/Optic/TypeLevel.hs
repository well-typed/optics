{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Optic.TypeLevel where

import Data.Kind
import Data.Type.Equality
import GHC.TypeLits
import Unsafe.Coerce

-- | A list of index types, used for indexed optics.
--
-- @since 0.4
data IxList = NoIx | WithIx Type | MultiIx Type Type [Type]

type family FromIxList (xs :: IxList) :: [Type] where
  FromIxList 'NoIx             = '[]
  FromIxList ('WithIx x)       = '[x]
  FromIxList ('MultiIx x y is) = x : y : is

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
    '(AppendList fs1 fs2, AppendList ops1 ops2)

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
type family Curry (xs :: IxList) (r :: Type) :: Type where
  Curry 'NoIx             r = r
  Curry ('WithIx x)       r = x -> r
  Curry ('MultiIx x y is) r = x -> y -> CurryList is r

type family CurryList (xs :: [Type]) (y :: Type) :: Type where
  CurryList '[]       y = y
  CurryList (x ': xs) y = x -> CurryList xs y

type family Append (xs :: IxList) (ys :: IxList) :: IxList where
  Append 'NoIx             ys                = ys
  Append  xs               'NoIx             = xs
  Append ('WithIx x)       ('WithIx y)       = 'MultiIx x y '[]
  Append ('WithIx x)       ('MultiIx y z is) = 'MultiIx x y (z : is)
  Append ('MultiIx x y is) ('WithIx z)       = 'MultiIx x y (AppendList is '[z])

-- | Append two type-level lists together.
type family AppendList (xs :: [k]) (ys :: [k]) :: [k] where
  AppendList '[]       ys  = ys
  AppendList (x ': xs) ys  = x ': AppendList xs ys

-- | Class that is inhabited by all type-level lists @xs@, providing the ability
-- to compose a function under @'Curry' xs@.
class CurryCompose xs where
  -- | Compose a function under @'Curry' xs@.  This generalises @('.')@ (aka
  -- 'fmap' for @(->)@) to work for curried functions with one argument for each
  -- type in the list.
  curryCompose :: (i -> j) -> Curry xs i -> Curry xs j

instance CurryCompose 'NoIx where
  curryCompose = id

instance CurryCompose ('WithIx x) where
  curryCompose ij f = ij . f

instance CurryComposeList is => CurryCompose ('MultiIx x y is) where
  curryCompose ij f x = curryComposeList @is ij . f x

class CurryComposeList xs where
  curryComposeList :: (i -> j) -> CurryList xs i -> CurryList xs j

instance CurryComposeList '[] where
  curryComposeList = id

instance CurryComposeList xs => CurryComposeList (x ': xs) where
  curryComposeList ij f = curryComposeList @xs ij . f

----------------------------------------
-- Indices

-- | In pseudo (dependent-)Haskell, provide a witness
--
-- @
-- foldr f (foldr f init xs) ys = foldr f init (ys ++ xs)
--    where f = (->)
-- @
class AppendIndices xs ys where
  appendIndices__
    :: proxy i
    -> CurryList xs (CurryList ys i) :~: CurryList (AppendList xs ys) i

instance AppendIndices '[] ys where
  appendIndices__ _ = Refl

instance
  (AppendList (x ': xs) ys ~ (x ': AppendList xs ys), AppendIndices xs ys
  ) => AppendIndices (x ': xs) ys where
  appendIndices__ i | Refl <- appendIndices__ @xs @ys i = Refl

appendIndices :: forall xs ys i. Curry xs (Curry ys i) :~: Curry (Append xs ys) i
appendIndices = unsafeCoerce Refl
-- Note: below is the proper definition (modulo IxList ~ [Type] isomorphism),
-- but it requires @AppendIndices xs ys@ in the context. We don't want to
-- propagate that constraint down into (%) and force users to experience it
-- since it's about internal details, so we trick the compiler with unsafeCoerce
-- that we always have the proof (which we do, but GHC can't see it and would
-- want to compute it itself).
--
-- appendIndices = appendIndices__ @xs @ys (Proxy @i)

----------------------------------------
-- Either

-- | If lhs is 'Right', return it. Otherwise check rhs.
type family FirstRight (m1 :: Either e a) (m2 :: Either e a) :: Either e a where
  FirstRight ('Right a) _ = 'Right a
  FirstRight          _ b = b

type family FromRight (def :: b) (e :: Either a b) :: b where
  FromRight _   ('Right b) = b
  FromRight def ('Left  _) = def

----------------------------------------
-- Errors

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
