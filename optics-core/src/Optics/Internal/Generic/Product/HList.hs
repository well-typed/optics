{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal.Generic.Product.HList
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive an isomorphism between a product type and a flat HList.
--
-----------------------------------------------------------------------------

module Optics.Internal.Generic.Product.HList
  ( GIsList(..)
  , HList (..)
  , type (++)
  , Elem
  , tupled
  , ListTuple (..)
  , TupleToList
  ) where

import Data.Kind    (Type)
import GHC.Generics
import GHC.Generics.Optics
import GHC.TypeLits

import Optics.Internal.Generic.Utils
import Optics.Equality
import Optics.Iso
import Optics.Re

data HList (as :: [Type]) where
  Nil :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)

infixr 5 :>

type family ((as :: [k]) ++ (bs :: [k])) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': as ++ bs

class Elem (as :: [(k, Type)]) (key :: k) (i :: Nat) a | as key -> i a
instance {-# OVERLAPPING #-} pos ~ 0 => Elem (a ': xs) key pos a
instance (Elem xs key i a, pos ~ (i + 1)) => Elem (x ': xs) key pos a

class GIsList
  (f :: Type -> Type)
  (g :: Type -> Type)
  (as :: [Type])
  (bs :: [Type]) | f -> as, g -> bs, bs f -> g, as g -> f where

  glist :: Iso (f x) (g x) (HList as) (HList bs)

instance
  ( GIsList l l' as as'
  , GIsList r r' bs bs'
  , Appending as bs cs as' bs' cs'
  , cs ~ (as ++ bs)
  , cs' ~ (as' ++ bs')
  ) => GIsList (l :*: r) (l' :*: r') cs cs' where

  glist = gproduct % pairing glist glist % appending
  {-# INLINE glist #-}

instance GIsList f g as bs => GIsList (M1 t meta f) (M1 t meta g) as bs where
  glist = _M1 % glist
  {-# INLINE glist #-}

instance GIsList (Rec0 a) (Rec0 b) '[a] '[b] where
  glist = _K1 % singleton
  {-# INLINE glist #-}

instance GIsList U1 U1 '[] '[] where
  glist = iso (const Nil) (const U1)
  {-# INLINE glist #-}

--------------------------------------------------------------------------------
-- | as ++ bs === cs
class Appending as bs cs as' bs' cs'
  | as bs cs cs'   -> as' bs'
  , as' bs' cs cs' -> as bs
  , as bs          -> cs
  , as' bs'        -> cs'
  where
  appending :: Iso (HList as, HList bs) (HList as', HList bs') (HList cs) (HList cs')

-- | [] ++ bs === bs
instance Appending '[] bs bs '[] bs' bs' where
  appending = iso snd (Nil,)
  {-# INLINE appending #-}

-- | (a : as) ++ bs === (a : cs)
instance
  Appending as bs cs as' bs' cs' -- as ++ bs == cs
  => Appending (a ': as) bs (a ': cs) (a' ': as') bs' (a' ': cs') where
  appending
    = pairing (re consing) equality -- ((a, as), bs)
    % assoc3                        -- (a, (as, bs))
    % pairing equality appending    -- (a, cs)
    % consing                       -- (a : cs)
  {-# INLINE appending #-}

singleton :: Iso a b (HList '[a]) (HList '[ b])
singleton = iso (:> Nil) (\(x :> _) -> x)
{-# INLINE singleton #-}

consing :: Iso (a, HList as) (b, HList bs) (HList (a ': as)) (HList (b ': bs))
consing = iso (\(x, xs) -> x :> xs) (\(x :> xs) -> (x, xs))
{-# INLINE consing #-}

--------------------------------------------------------------------------------
-- * Convert tuples to/from HLists

tupled :: (ListTuple a as, ListTuple b bs) => Iso (HList as) (HList bs) a b
tupled = iso listToTuple tupleToList
{-# INLINE tupled #-}

class ListTuple (tuple :: Type) (as :: [Type]) | as -> tuple where
  type ListToTuple as :: Type
  tupleToList :: tuple -> HList as
  listToTuple :: HList as -> tuple

instance ListTuple () '[] where
  type ListToTuple '[] = ()
  tupleToList _ = Nil
  listToTuple _ = ()

instance ListTuple a '[a] where
  type ListToTuple '[a] = a
  tupleToList a
    = a :> Nil
  listToTuple (a :> Nil)
    = a

instance ListTuple (a, b) '[a, b] where
  type ListToTuple '[a, b] = (a, b)
  tupleToList (a, b)
    = a :> b :> Nil
  listToTuple (a :> b :> Nil)
    = (a, b)

instance ListTuple (a, b, c) '[a, b, c] where
  type ListToTuple '[a, b, c] = (a, b, c)
  tupleToList (a, b, c)
    = a :> b :> c :> Nil
  listToTuple (a :> b :> c :> Nil)
    = (a, b, c)

instance ListTuple (a, b, c, d) '[a, b, c, d] where
  type ListToTuple '[a, b, c, d] = (a, b, c, d)
  tupleToList (a, b, c, d)
    = a :> b :> c :> d:> Nil
  listToTuple (a :> b :> c :> d :> Nil)
    = (a, b, c, d)

instance ListTuple (a, b, c, d, e) '[a, b, c, d, e] where
  type ListToTuple '[a, b, c, d, e] = (a, b, c, d, e)
  tupleToList (a, b, c, d, e)
    = a :> b :> c :> d:> e :> Nil
  listToTuple (a :> b :> c :> d :> e :> Nil)
    = (a, b, c, d, e)

instance ListTuple (a, b, c, d, e, f) '[a, b, c, d, e, f] where
  type ListToTuple '[a, b, c, d, e, f] = (a, b, c, d, e, f)
  tupleToList (a, b, c, d, e, f)
    = a :> b :> c :> d:> e :> f :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> Nil)
    = (a, b, c, d, e, f)

instance ListTuple (a, b, c, d, e, f, g) '[a, b, c, d, e, f, g] where
  type ListToTuple '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  tupleToList (a, b, c, d, e, f, g)
    = a :> b :> c :> d:> e :> f :> g :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> Nil)
    = (a, b, c, d, e, f, g)

instance ListTuple (a, b, c, d, e, f, g, h) '[a, b, c, d, e, f, g, h] where
  type ListToTuple '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  tupleToList (a, b, c, d, e, f, g, h)
    = a :> b :> c :> d:> e :> f :> g :> h :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> Nil)
    = (a, b, c, d, e, f, g, h)

instance ListTuple (a, b, c, d, e, f, g, h, j) '[a, b, c, d, e, f, g, h, j] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j] = (a, b, c, d, e, f, g, h, j)
  tupleToList (a, b, c, d, e, f, g, h, j)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> Nil)
    = (a, b, c, d, e, f, g, h, j)

instance ListTuple (a, b, c, d, e, f, g, h, j, k) '[a, b, c, d, e, f, g, h, j, k] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k] = (a, b, c, d, e, f, g, h, j, k)
  tupleToList (a, b, c, d, e, f, g, h, j, k)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> Nil)
    = (a, b, c, d, e, f, g, h, j, k)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l) '[a, b, c, d, e, f, g, h, j, k, l] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l] = (a, b, c, d, e, f, g, h, j, k, l)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m) '[a, b, c, d, e, f, g, h, j, k, l, m] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m] = (a, b, c, d, e, f, g, h, j, k, l, m)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n) '[a, b, c, d, e, f, g, h, j, k, l, m, n] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m, n] = (a, b, c, d, e, f, g, h, j, k, l, m, n)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o) '[a, b, c, d, e, f, g, h, j, k, l, m, n, o] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m, n, o] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p) '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q) '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r) '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r)

instance ListTuple (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s) '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s] where
  type ListToTuple '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s] = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s)
  tupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s)
    = a :> b :> c :> d:> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> s :> Nil
  listToTuple (a :> b :> c :> d :> e :> f :> g :> h :> j :> k :> l :> m :> n :> o :> p :> q :> r :> s :> Nil)
    = (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s)

type family TupleToList a where
  TupleToList () = '[]
  TupleToList (a, b) = '[a, b]
  TupleToList (a, b, c) = '[a, b, c]
  TupleToList (a, b, c, d) = '[a, b, c, d]
  TupleToList (a, b, c, d, e) = '[a, b, c, d, e]
  TupleToList (a, b, c, d, e, f) = '[a, b, c, d, e, f]
  TupleToList (a, b, c, d, e, f, g) = '[a, b, c, d, e, f, g]
  TupleToList (a, b, c, d, e, f, g, h) = '[a, b, c, d, e, f, g, h]
  TupleToList (a, b, c, d, e, f, g, h, j) = '[a, b, c, d, e, f, g, h, j]
  TupleToList (a, b, c, d, e, f, g, h, j, k) = '[a, b, c, d, e, f, g, h, j, k]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l) = '[a, b, c, d, e, f, g, h, j, k, l]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m) = '[a, b, c, d, e, f, g, h, j, k, l, m]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n) = '[a, b, c, d, e, f, g, h, j, k, l, m, n]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o) = '[a, b, c, d, e, f, g, h, j, k, l, m, n, o]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p) = '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q) = '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r) = '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r]
  TupleToList (a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s) = '[a, b, c, d, e, f, g, h, j, k, l, m, n, o, p, q, r, s]
  TupleToList a = '[a]
