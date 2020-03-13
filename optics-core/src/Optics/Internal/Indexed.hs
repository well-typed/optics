{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of indexed optics.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Indexed where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Functor.Constant
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum
import Data.Ix
import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Monoid hiding (Product, Sum)
import Data.Proxy
import Data.Tree
import Data.Void
import GHC.Generics
import GHC.TypeLits
import qualified Data.Array as Array
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Data.Profunctor.Indexed

import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Show useful error message when a function expects optics without indices.
class is ~ NoIx => AcceptsEmptyIndices (f :: Symbol) (is :: IxList)

instance
  ( TypeError
    ('Text "‘" ':<>: 'Text f ':<>: 'Text "’ accepts only optics with no indices")
  , (x ': xs) ~ NoIx
  ) => AcceptsEmptyIndices f (x ': xs)

instance AcceptsEmptyIndices f '[]

-- | Check whether a list of indices is not empty and generate sensible error
-- message if it's not.
class NonEmptyIndices (is :: IxList)

instance
  ( TypeError
    ('Text "Indexed optic is expected")
  ) => NonEmptyIndices '[]

instance NonEmptyIndices (x ': xs)

-- | Generate sensible error messages in case a user tries to pass either an
-- unindexed optic or indexed optic with unflattened indices where indexed optic
-- with a single index is expected.
class is ~ '[i] => HasSingleIndex (is :: IxList) (i :: Type)

instance HasSingleIndex '[i] i

instance
  ( TypeError
    ('Text "Indexed optic is expected")
  , '[] ~ '[i]
  ) => HasSingleIndex '[] i

instance
  ( TypeError
    ('Text "Use (<%>) or icompose to combine indices of type "
     ':<>: ShowTypes is)
  , is ~ '[i1, i2]
  , is ~ '[i]
  ) => HasSingleIndex '[i1, i2] i

instance
  ( TypeError
    ('Text "Use icompose3 to combine indices of type "
     ':<>: ShowTypes is)
  , is ~ '[i1, i2, i3]
  , is ~ '[i]
  ) => HasSingleIndex [i1, i2, i3] i

instance
  ( TypeError
    ('Text "Use icompose4 to combine indices of type "
     ':<>: ShowTypes is)
  , is ~ '[i1, i2, i3, i4]
  , is ~ '[i]
  ) => HasSingleIndex '[i1, i2, i3, i4] i

instance
  ( TypeError
    ('Text "Use icompose5 to flatten indices of type "
     ':<>: ShowTypes is)
  , is ~ '[i1, i2, i3, i4, i5]
  , is ~ '[i]
  ) => HasSingleIndex '[i1, i2, i3, i4, i5] i

instance
  ( TypeError
    ('Text "Use icomposeN to flatten indices of type "
     ':<>: ShowTypes is)
  , is ~ (i1 ': i2 ': i3 ': i4 ': i5 ': i6 : is')
  , is ~ '[i]
  ) => HasSingleIndex (i1 ': i2 ': i3 ': i4 ': i5 ': i6 ': is') i

----------------------------------------
-- Helpers for HasSingleIndex

type family ShowTypes (types :: [Type]) :: ErrorMessage where
  ShowTypes '[i]      = QuoteType i
  ShowTypes '[i, j]   = QuoteType i ':<>: 'Text " and " ':<>: QuoteType j
  ShowTypes (i ': is) = QuoteType i ':<>: 'Text ", " ':<>: ShowTypes is

----------------------------------------

data IntT f a = IntT {-# UNPACK #-} !Int (f a)

unIntT :: IntT f a -> f a
unIntT (IntT _ fa) = fa

newtype Indexing f a = Indexing { runIndexing :: Int -> IntT f a }

instance Functor f => Functor (Indexing f) where
  fmap f (Indexing m) = Indexing $ \i -> case m i of
    IntT j x -> IntT j (fmap f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Indexing f) where
  pure x = Indexing $ \i -> IntT i (pure x)
  {-# INLINE pure #-}
  Indexing mf <*> Indexing ma = Indexing $ \i -> case mf i of
    IntT j ff -> case ma j of
       IntT k fa -> IntT k (ff <*> fa)
  {-# INLINE (<*>) #-}

-- | Index a traversal by position of visited elements.
indexing
  :: ((a -> Indexing f b) -> s -> Indexing f t)
  -> ((Int -> a -> f b) -> s -> f t)
indexing l iafb s =
  unIntT $ runIndexing (l (\a -> Indexing (\i -> IntT (i + 1) (iafb i a))) s) 0
{-# INLINE indexing #-}

----------------------------------------

-- | Construct a conjoined indexed optic that provides a separate code path when
-- used without indices. Useful for defining indexed optics that are as
-- efficient as their unindexed equivalents when used without indices.
--
-- /Note:/ @'conjoined' f g@ is well-defined if and only if @f ≡
-- 'Optics.Indexed.Core.noIx' g@.
conjoined
  :: is `HasSingleIndex` i
  => Optic k NoIx s t a b
  -> Optic k is   s t a b
  -> Optic k is   s t a b
conjoined (Optic f) (Optic g) = Optic (conjoined__ f g)
{-# INLINE conjoined #-}

----------------------------------------

-- | Class for 'Functor's that have an additional read-only index available.
class Functor f => FunctorWithIndex i f | f -> i where
  imap :: (i -> a -> b) -> f a -> f b
  default imap
    :: TraversableWithIndex i f => (i -> a -> b) -> f a -> f b
  imap f = runIxFunArrow (iwander itraverse (IxFunArrow f)) id
  {-# INLINE imap #-}

-- | Class for 'Foldable's that have an additional read-only index available.
class (FunctorWithIndex i f, Foldable f
      ) => FoldableWithIndex i f | f -> i where
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m
  default ifoldMap
    :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
  ifoldMap f = runIxForget (iwander itraverse (IxForget f)) id
  {-# INLINE ifoldMap #-}

  ifoldr :: (i -> a -> b -> b) -> b -> f a -> b
  ifoldr iabb b0 = (\e -> appEndo e b0) . ifoldMap (\i -> Endo #. iabb i)
  {-# INLINE ifoldr #-}

  ifoldl' :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl' ibab b0 s = ifoldr (\i a bb b -> bb $! ibab i b a) id s b0
  {-# INLINE ifoldl' #-}

-- | Traverse 'FoldableWithIndex' ignoring the results.
itraverse_ :: (FoldableWithIndex i t, Applicative f) => (i -> a -> f b) -> t a -> f ()
itraverse_ f = runTraversed . ifoldMap (\i -> Traversed #. f i)
{-# INLINE itraverse_ #-}

-- | Flipped 'itraverse_'.
ifor_ :: (FoldableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f ()
ifor_ = flip itraverse_
{-# INLINE ifor_ #-}

-- | Class for 'Traversable's that have an additional read-only index available.
class (FoldableWithIndex i t, Traversable t
      ) => TraversableWithIndex i t | t -> i where
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

-- | Flipped 'itraverse'
ifor :: (TraversableWithIndex i t, Applicative f) => t a -> (i -> a -> f b) -> f (t b)
ifor = flip itraverse
{-# INLINE ifor #-}

----------------------------------------
-- Instances

-- Identity

instance FunctorWithIndex () Identity where
  imap f (Identity a) = Identity (f () a)
  {-# INLINE imap #-}

instance FoldableWithIndex () Identity where
  ifoldMap f (Identity a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () Identity where
  itraverse f (Identity a) = Identity <$> f () a
  {-# INLINE itraverse #-}

-- Const

instance FunctorWithIndex Void (Const e) where
  imap _ (Const a) = Const a
  {-# INLINE imap #-}

instance FoldableWithIndex Void (Const e) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (Const e) where
  itraverse _ (Const a) = pure (Const a)
  {-# INLINE itraverse #-}

-- Constant

instance FunctorWithIndex Void (Constant e) where
  imap _ (Constant a) = Constant a
  {-# INLINE imap #-}

instance FoldableWithIndex Void (Constant e) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (Constant e) where
  itraverse _ (Constant a) = pure (Constant a)
  {-# INLINE itraverse #-}

-- (,) k

instance FunctorWithIndex k ((,) k) where
  imap f (k, a) = (k, f k a)
  {-# INLINE imap #-}

instance FoldableWithIndex k ((,) k) where
  ifoldMap = uncurry'
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex k ((,) k) where
  itraverse f (k, a) = (,) k <$> f k a
  {-# INLINE itraverse #-}

-- (->) r

instance FunctorWithIndex r ((->) r) where
  imap f g x = f x (g x)
  {-# INLINE imap #-}

-- []

instance FunctorWithIndex Int []
instance FoldableWithIndex Int []
instance TraversableWithIndex Int [] where
  -- Faster than @indexing traverse@, also best for folds and setters.
  itraverse f = traverse (uncurry' f) . Prelude.zip [0..]
  {-# INLINE itraverse #-}

-- ZipList

instance FunctorWithIndex Int ZipList
instance FoldableWithIndex Int ZipList
instance TraversableWithIndex Int ZipList where
  itraverse f (ZipList xs) = ZipList <$> itraverse f xs
  {-# INLINE itraverse #-}

-- NonEmpty

instance FunctorWithIndex Int NonEmpty
instance FoldableWithIndex Int NonEmpty
instance TraversableWithIndex Int NonEmpty where
  itraverse f ~(a :| as) =
    (:|) <$> f 0 a <*> traverse (uncurry' f) (Prelude.zip [1..] as)
  {-# INLINE itraverse #-}

-- Maybe

instance FunctorWithIndex () Maybe where
  imap f = fmap (f ())
  {-# INLINE imap #-}
instance FoldableWithIndex () Maybe where
  ifoldMap f = foldMap (f ())
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex () Maybe where
  itraverse f = traverse (f ())
  {-# INLINE itraverse #-}

-- Seq

-- | The position in the 'Seq.Seq' is available as the index.
instance FunctorWithIndex Int Seq.Seq where
  imap = Seq.mapWithIndex
  {-# INLINE imap #-}
instance FoldableWithIndex Int Seq.Seq where
#if MIN_VERSION_containers(0,5,8)
  ifoldMap = Seq.foldMapWithIndex
#else
  ifoldMap f = ifoldr (\i -> mappend . f i) mempty
#endif
  {-# INLINE ifoldMap #-}

  ifoldr = Seq.foldrWithIndex
  {-# INLINE ifoldr #-}

instance TraversableWithIndex Int Seq.Seq where
#if MIN_VERSION_containers(0,6,0)
  itraverse = Seq.traverseWithIndex
#else
  -- Much faster than Seq.traverseWithIndex for containers < 0.6.0, see
  -- https://github.com/haskell/containers/issues/603.
  itraverse f = sequenceA . Seq.mapWithIndex f
#endif
  {-# INLINE itraverse #-}

-- IntMap

instance FunctorWithIndex Int IntMap.IntMap where
  imap = IntMap.mapWithKey
  {-# INLINE imap #-}
instance FoldableWithIndex Int IntMap.IntMap where
  ifoldMap = IntMap.foldMapWithKey
  ifoldr   = IntMap.foldrWithKey
  ifoldl'  = IntMap.foldlWithKey' . flip
  {-# INLINE ifoldMap #-}
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
instance TraversableWithIndex Int IntMap.IntMap where
  itraverse = IntMap.traverseWithKey
  {-# INLINE itraverse #-}

-- Map

instance FunctorWithIndex k (Map.Map k) where
  imap = Map.mapWithKey
  {-# INLINE imap #-}
instance FoldableWithIndex k (Map.Map k) where
  ifoldMap = Map.foldMapWithKey
  ifoldr   = Map.foldrWithKey
  ifoldl'  = Map.foldlWithKey' . flip
  {-# INLINE ifoldMap #-}
  {-# INLINE ifoldr #-}
  {-# INLINE ifoldl' #-}
instance TraversableWithIndex k (Map.Map k) where
  itraverse = Map.traverseWithKey
  {-# INLINE itraverse #-}

-- Array

instance Ix i => FunctorWithIndex i (Array.Array i) where
  imap f arr = Array.listArray (Array.bounds arr)
    . fmap (uncurry' f) $ Array.assocs arr
  {-# INLINE imap #-}

instance Ix i => FoldableWithIndex i (Array.Array i) where
  ifoldMap f = foldMap (uncurry' f) . Array.assocs
  {-# INLINE ifoldMap #-}

instance Ix i => TraversableWithIndex i (Array.Array i) where
  itraverse f arr = Array.listArray (Array.bounds arr)
    <$> traverse (uncurry' f) (Array.assocs arr)
  {-# INLINE itraverse #-}

-- Compose

instance (FunctorWithIndex i f, FunctorWithIndex j g
         ) => FunctorWithIndex (i, j) (Compose f g) where
  imap f (Compose fg) = Compose $ imap (\k -> imap (f . (,) k)) fg
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g
         ) => FoldableWithIndex (i, j) (Compose f g) where
  ifoldMap f (Compose fg) = ifoldMap (\k -> ifoldMap (f . (,) k)) fg
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g
         ) => TraversableWithIndex (i, j) (Compose f g) where
  itraverse f (Compose fg) =
    Compose <$> itraverse (\k -> itraverse (f . (,) k)) fg
  {-# INLINE itraverse #-}

-- Sum

instance (FunctorWithIndex i f, FunctorWithIndex j g
         ) => FunctorWithIndex (Either i j) (Sum f g) where
  imap q (InL fa) = InL (imap (q . Left)  fa)
  imap q (InR ga) = InR (imap (q . Right) ga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g
         ) => FoldableWithIndex (Either i j) (Sum f g) where
  ifoldMap q (InL fa) = ifoldMap (q . Left)  fa
  ifoldMap q (InR ga) = ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g
         ) => TraversableWithIndex (Either i j) (Sum f g) where
  itraverse q (InL fa) = InL <$> itraverse (q . Left)  fa
  itraverse q (InR ga) = InR <$> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

-- Product

instance (FunctorWithIndex i f, FunctorWithIndex j g
         ) => FunctorWithIndex (Either i j) (Product f g) where
  imap f (Pair a b) = Pair (imap (f . Left) a) (imap (f . Right) b)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g
         ) => FoldableWithIndex (Either i j) (Product f g) where
  ifoldMap f (Pair a b) =
    ifoldMap (f . Left) a `mappend` ifoldMap (f . Right) b
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g
         ) => TraversableWithIndex (Either i j) (Product f g) where
  itraverse f (Pair a b) =
    Pair <$> itraverse (f . Left) a <*> itraverse (f . Right) b
  {-# INLINE itraverse #-}

-- Tree

instance FunctorWithIndex [Int] Tree where
  imap f (Node a as) = Node (f [] a) $ imap (\i -> imap (f . (:) i)) as
  {-# INLINE imap #-}

instance FoldableWithIndex [Int] Tree where
  ifoldMap f (Node a as) =
    f [] a `mappend` ifoldMap (\i -> ifoldMap (f . (:) i)) as
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex [Int] Tree where
  itraverse f (Node a as) =
    Node <$> f [] a <*> itraverse (\i -> itraverse (f . (:) i)) as
  {-# INLINE itraverse #-}

-- Proxy

instance FunctorWithIndex Void Proxy where
  imap _ Proxy = Proxy
  {-# INLINE imap #-}

instance FoldableWithIndex Void Proxy where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void Proxy where
  itraverse _ _ = pure Proxy
  {-# INLINE itraverse #-}

-- Backwards

instance FunctorWithIndex i f => FunctorWithIndex i (Backwards f) where
  imap f  = Backwards . imap f . forwards
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Backwards f) where
  ifoldMap f = ifoldMap f . forwards
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Backwards f) where
  itraverse f = fmap Backwards . itraverse f . forwards
  {-# INLINE itraverse #-}

-- Reverse

instance FunctorWithIndex i f => FunctorWithIndex i (Reverse f) where
  imap f = Reverse . imap f . getReverse
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Reverse f) where
  ifoldMap f = getDual . ifoldMap (\i -> Dual #. f i) . getReverse
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Reverse f) where
  itraverse f =
    fmap Reverse . forwards . itraverse (\i -> Backwards . f i) . getReverse
  {-# INLINE itraverse #-}

-- IdentityT

instance FunctorWithIndex i m => FunctorWithIndex i (IdentityT m) where
  imap f (IdentityT m) = IdentityT $ imap f m
  {-# INLINE imap #-}

instance FoldableWithIndex i m => FoldableWithIndex i (IdentityT m) where
  ifoldMap f (IdentityT m) = ifoldMap f m
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i m => TraversableWithIndex i (IdentityT m) where
  itraverse f (IdentityT m) = IdentityT <$> itraverse f m
  {-# INLINE itraverse #-}

-- ReaderT

instance FunctorWithIndex i m => FunctorWithIndex (e, i) (ReaderT e m) where
  imap f (ReaderT m) = ReaderT $ \k -> imap (f . (,) k) (m k)
  {-# INLINE imap #-}

-- Generics

instance FunctorWithIndex Void V1 where
  imap _ v = v `seq` undefined
  {-# INLINE imap #-}

instance FoldableWithIndex Void V1 where
  ifoldMap _ v = v `seq` undefined

instance TraversableWithIndex Void V1 where
  itraverse _ v = v `seq` undefined

instance FunctorWithIndex Void U1 where
  imap _ U1 = U1
  {-# INLINE imap #-}

instance FoldableWithIndex Void U1 where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void U1 where
  itraverse _ U1 = pure U1
  {-# INLINE itraverse #-}

instance FunctorWithIndex () Par1 where
  imap f = fmap (f ())
  {-# INLINE imap #-}

instance FoldableWithIndex () Par1 where
  ifoldMap f (Par1 a) = f () a
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex () Par1 where
  itraverse f (Par1 a) = Par1 <$> f () a
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g
         ) => FunctorWithIndex (i, j) (f :.: g) where
  imap q (Comp1 fga) = Comp1 (imap (\k -> imap (q . (,) k)) fga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g
         ) => FoldableWithIndex (i, j) (f :.: g) where
  ifoldMap q (Comp1 fga) = ifoldMap (\k -> ifoldMap (q . (,) k)) fga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g
         ) => TraversableWithIndex (i, j) (f :.: g) where
  itraverse q (Comp1 fga) =
    Comp1 <$> itraverse (\k -> itraverse (q . (,) k)) fga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g
         ) => FunctorWithIndex (Either i j) (f :*: g) where
  imap q (fa :*: ga) = imap (q . Left) fa :*: imap (q . Right) ga
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g
         ) => FoldableWithIndex (Either i j) (f :*: g) where
  ifoldMap q (fa :*: ga) =
    ifoldMap (q . Left) fa `mappend` ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g
         ) => TraversableWithIndex (Either i j) (f :*: g) where
  itraverse q (fa :*: ga) =
    (:*:) <$> itraverse (q . Left) fa <*> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance (FunctorWithIndex i f, FunctorWithIndex j g
         ) => FunctorWithIndex (Either i j) (f :+: g) where
  imap q (L1 fa) = L1 (imap (q . Left) fa)
  imap q (R1 ga) = R1 (imap (q . Right) ga)
  {-# INLINE imap #-}

instance (FoldableWithIndex i f, FoldableWithIndex j g
         ) => FoldableWithIndex (Either i j) (f :+: g) where
  ifoldMap q (L1 fa) = ifoldMap (q . Left) fa
  ifoldMap q (R1 ga) = ifoldMap (q . Right) ga
  {-# INLINE ifoldMap #-}

instance (TraversableWithIndex i f, TraversableWithIndex j g
         ) => TraversableWithIndex (Either i j) (f :+: g) where
  itraverse q (L1 fa) = L1 <$> itraverse (q . Left) fa
  itraverse q (R1 ga) = R1 <$> itraverse (q . Right) ga
  {-# INLINE itraverse #-}

instance FunctorWithIndex i f => FunctorWithIndex i (Rec1 f) where
  imap q (Rec1 f) = Rec1 (imap q f)
  {-# INLINE imap #-}

instance FoldableWithIndex i f => FoldableWithIndex i (Rec1 f) where
  ifoldMap q (Rec1 f) = ifoldMap q f
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex i f => TraversableWithIndex i (Rec1 f) where
  itraverse q (Rec1 f) = Rec1 <$> itraverse q f
  {-# INLINE itraverse #-}

instance FunctorWithIndex Void (K1 i c) where
  imap _ (K1 c) = K1 c
  {-# INLINE imap #-}

instance FoldableWithIndex Void (K1 i c) where
  ifoldMap _ _ = mempty
  {-# INLINE ifoldMap #-}

instance TraversableWithIndex Void (K1 i c) where
  itraverse _ (K1 a) = pure (K1 a)
  {-# INLINE itraverse #-}
