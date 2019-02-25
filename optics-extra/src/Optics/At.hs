{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Optics.At
  (
  -- * At
    At(..)
  , sans
  -- * Ixed
  , Index
  , IxValue
  , Ixed(ix)
  , ixAt
  -- * Contains
  , Contains(..)
  ) where

import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.Hashable
import Data.Int
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Vector as Vector hiding (indexed)
import Data.Vector.Primitive as Prim
import Data.Vector.Storable as Storable
import Data.Vector.Unboxed as Unboxed hiding (indexed)
import Data.Word

import Optics.Core

type instance Index (HashSet a) = a
type instance Index (HashMap k a) = k
type instance Index (Vector.Vector a) = Int
type instance Index (Prim.Vector a) = Int
type instance Index (Storable.Vector a) = Int
type instance Index (Unboxed.Vector a) = Int
type instance Index StrictT.Text = Int
type instance Index LazyT.Text = Int64
type instance Index StrictB.ByteString = Int
type instance Index LazyB.ByteString = Int64

-- Contains

instance (Eq a, Hashable a) => Contains (HashSet a) where
  contains k = lensVL $ \f s -> f (HashSet.member k s) <&> \b ->
    if b then HashSet.insert k s else HashSet.delete k s
  {-# INLINE contains #-}

-- Ixed

type instance IxValue (HashMap k a) = a
-- Default implementation uses HashMap.alterF
instance (Eq k, Hashable k) => Ixed (HashMap k a)

type instance IxValue (HashSet k) = ()
instance (Eq k, Hashable k) => Ixed (HashSet k) where
  ix k = atraversalVL $ \point f m ->
    if HashSet.member k m
    then f () <&> \() -> HashSet.insert k m
    else point m
  {-# INLINE ix #-}

type instance IxValue (Vector.Vector a) = a
instance Ixed (Vector.Vector a) where
  ix i = atraversalVL $ \point f v ->
    if 0 <= i && i < Vector.length v
    then f (v Vector.! i) <&> \a -> v Vector.// [(i, a)]
    else point v
  {-# INLINE ix #-}

type instance IxValue (Prim.Vector a) = a
instance Prim a => Ixed (Prim.Vector a) where
  ix i = atraversalVL $ \point f v ->
    if 0 <= i && i < Prim.length v
    then f (v Prim.! i) <&> \a -> v Prim.// [(i, a)]
    else point v
  {-# INLINE ix #-}

type instance IxValue (Storable.Vector a) = a
instance Storable a => Ixed (Storable.Vector a) where
  ix i = atraversalVL $ \point f v ->
    if 0 <= i && i < Storable.length v
    then f (v Storable.! i) <&> \a -> v Storable.// [(i, a)]
    else point v
  {-# INLINE ix #-}

type instance IxValue (Unboxed.Vector a) = a
instance Unbox a => Ixed (Unboxed.Vector a) where
  ix i = atraversalVL $ \point f v ->
    if 0 <= i && i < Unboxed.length v
    then f (v Unboxed.! i) <&> \a -> v Unboxed.// [(i, a)]
    else point v
  {-# INLINE ix #-}

type instance IxValue StrictT.Text = Char
instance Ixed StrictT.Text where
  ix e = atraversalVL $ \point f s ->
    case StrictT.splitAt e s of
      (l, mr) -> case StrictT.uncons mr of
        Nothing      -> point s
        Just (c, xs) -> f c <&> \d -> StrictT.concat [l, StrictT.singleton d, xs]
  {-# INLINE ix #-}

type instance IxValue LazyT.Text = Char
instance Ixed LazyT.Text where
  ix e = atraversalVL $ \point f s ->
    case LazyT.splitAt e s of
      (l, mr) -> case LazyT.uncons mr of
        Nothing      -> point s
        Just (c, xs) -> f c <&> \d -> LazyT.append l (LazyT.cons d xs)
  {-# INLINE ix #-}

type instance IxValue StrictB.ByteString = Word8
instance Ixed StrictB.ByteString where
  ix e = atraversalVL $ \point f s ->
    case StrictB.splitAt e s of
      (l, mr) -> case StrictB.uncons mr of
        Nothing      -> point s
        Just (c, xs) -> f c <&> \d -> StrictB.concat [l, StrictB.singleton d, xs]
  {-# INLINE ix #-}

type instance IxValue LazyB.ByteString = Word8
instance Ixed LazyB.ByteString where
  -- TODO: we could be lazier, returning each chunk as it is passed
  ix e = atraversalVL $ \point f s ->
    case LazyB.splitAt e s of
      (l, mr) -> case LazyB.uncons mr of
        Nothing      -> point s
        Just (c, xs) -> f c <&> \d -> LazyB.append l (LazyB.cons d xs)
  {-# INLINE ix #-}

-- At

instance (Eq k, Hashable k) => At (HashMap k a) where
#if MIN_VERSION_unordered_containers(0,2,9)
  at k = lensVL $ \f -> HashMap.alterF f k
#else
  at k = lensVL $ \f m ->
    let mv = HashMap.lookup k m
    in f mv <&> \r -> case r of
      Nothing -> maybe m (const (HashMap.delete k m)) mv
      Just v' -> HashMap.insert k v' m
#endif
  {-# INLINE at #-}

instance (Eq k, Hashable k) => At (HashSet k) where
  at k = lensVL $ \f m ->
    let mv = if HashSet.member k m
             then Just ()
             else Nothing
    in f mv <&> \r -> case r of
      Nothing -> maybe m (const (HashSet.delete k m)) mv
      Just () -> HashSet.insert k m
  {-# INLINE at #-}
