-- |
-- Module: Optics.Cons
-- Description: Optics to access the left or right element of a container.
--
-- This module defines the 'Cons' and 'Snoc' classes, which provide 'Prism's for
-- the leftmost and rightmost elements of a container, respectively.
--
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Optics.Cons
  (
  -- * Cons
    Cons(..)
  , (<|)
  , cons
  , uncons
  , _head, _tail
  , pattern (:<)
  -- * Snoc
  , Snoc(..)
  , (|>)
  , snoc
  , unsnoc
  , _init, _last
  , pattern (:>)
  ) where

import Data.Vector (Vector)
import Data.Vector.Primitive (Prim)
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word
import qualified Data.ByteString as StrictB
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.Text as StrictT
import qualified Data.Text.Lazy as LazyT
import qualified Data.Vector as Vector
import qualified Data.Vector.Primitive as Prim
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unbox

import Optics.Core

-- Cons

instance Cons StrictB.ByteString StrictB.ByteString Word8 Word8 where
  _Cons = prism' (uncurry StrictB.cons) StrictB.uncons
  {-# INLINE _Cons #-}

instance Cons LazyB.ByteString LazyB.ByteString Word8 Word8 where
  _Cons = prism' (uncurry LazyB.cons) LazyB.uncons
  {-# INLINE _Cons #-}

instance Cons StrictT.Text StrictT.Text Char Char where
  _Cons = prism' (uncurry StrictT.cons) StrictT.uncons
  {-# INLINE _Cons #-}

instance Cons LazyT.Text LazyT.Text Char Char where
  _Cons = prism' (uncurry LazyT.cons) LazyT.uncons
  {-# INLINE _Cons #-}

instance Cons (Vector a) (Vector b) a b where
  _Cons = prism (uncurry Vector.cons) $ \v ->
    if Vector.null v
    then Left Vector.empty
    else Right (Vector.unsafeHead v, Vector.unsafeTail v)
  {-# INLINE _Cons #-}

instance (Prim a, Prim b) => Cons (Prim.Vector a) (Prim.Vector b) a b where
  _Cons = prism (uncurry Prim.cons) $ \v ->
    if Prim.null v
    then Left Prim.empty
    else Right (Prim.unsafeHead v, Prim.unsafeTail v)
  {-# INLINE _Cons #-}

instance (Storable a, Storable b) => Cons (Storable.Vector a) (Storable.Vector b) a b where
  _Cons = prism (uncurry Storable.cons) $ \v ->
    if Storable.null v
    then Left Storable.empty
    else Right (Storable.unsafeHead v, Storable.unsafeTail v)
  {-# INLINE _Cons #-}

instance (Unbox a, Unbox b) => Cons (Unbox.Vector a) (Unbox.Vector b) a b where
  _Cons = prism (uncurry Unbox.cons) $ \v ->
    if Unbox.null v
    then Left Unbox.empty
    else Right (Unbox.unsafeHead v, Unbox.unsafeTail v)
  {-# INLINE _Cons #-}

-- Snoc

instance Snoc (Vector a) (Vector b) a b where
  _Snoc = prism (uncurry Vector.snoc) $ \v -> if Vector.null v
    then Left Vector.empty
    else Right (Vector.unsafeInit v, Vector.unsafeLast v)
  {-# INLINE _Snoc #-}

instance (Prim a, Prim b) => Snoc (Prim.Vector a) (Prim.Vector b) a b where
  _Snoc = prism (uncurry Prim.snoc) $ \v -> if Prim.null v
    then Left Prim.empty
    else Right (Prim.unsafeInit v, Prim.unsafeLast v)
  {-# INLINE _Snoc #-}

instance (Storable a, Storable b) => Snoc (Storable.Vector a) (Storable.Vector b) a b where
  _Snoc = prism (uncurry Storable.snoc) $ \v -> if Storable.null v
    then Left Storable.empty
    else Right (Storable.unsafeInit v, Storable.unsafeLast v)
  {-# INLINE _Snoc #-}

instance (Unbox a, Unbox b) => Snoc (Unbox.Vector a) (Unbox.Vector b) a b where
  _Snoc = prism (uncurry Unbox.snoc) $ \v -> if Unbox.null v
    then Left Unbox.empty
    else Right (Unbox.unsafeInit v, Unbox.unsafeLast v)
  {-# INLINE _Snoc #-}

instance Snoc StrictB.ByteString StrictB.ByteString Word8 Word8 where
  _Snoc = prism (uncurry StrictB.snoc) $ \v -> if StrictB.null v
    then Left StrictB.empty
    else Right (StrictB.init v, StrictB.last v)
  {-# INLINE _Snoc #-}

instance Snoc LazyB.ByteString LazyB.ByteString Word8 Word8 where
  _Snoc = prism (uncurry LazyB.snoc) $ \v -> if LazyB.null v
    then Left LazyB.empty
    else Right (LazyB.init v, LazyB.last v)
  {-# INLINE _Snoc #-}

instance Snoc StrictT.Text StrictT.Text Char Char where
  _Snoc = prism (uncurry StrictT.snoc) $ \v -> if StrictT.null v
    then Left StrictT.empty
    else Right (StrictT.init v, StrictT.last v)
  {-# INLINE _Snoc #-}

instance Snoc LazyT.Text LazyT.Text Char Char where
  _Snoc = prism (uncurry LazyT.snoc) $ \v -> if LazyT.null v
    then Left LazyT.empty
    else Right (LazyT.init v, LazyT.last v)
  {-# INLINE _Snoc #-}
