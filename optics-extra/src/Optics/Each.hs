{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Optics.Each
  (
  -- * Each
    Each(..)
  ) where

import Data.ByteString as SB
import Data.ByteString.Lazy as LB
import Data.HashMap.Lazy as HashMap
import Data.Text as ST
import Data.Text.Lazy as LT
import Data.Text.Optics (text)
import Data.Vector.Generic.Optics (vectorTraverse)
import Data.Vector.Primitive (Prim)
import Data.Vector.Storable (Storable)
import Data.Vector.Unboxed (Unbox)
import Data.Word
import qualified Data.Vector as Vector
import qualified Data.Vector.Primitive as Prim
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed

import Optics.Core
import Optics.Extra.Internal.ByteString

-- Extra instances

-- | @'each' :: 'Traversal' ('HashMap' c a) ('HashMap' c b) a b@
instance (c ~ d) => Each (HashMap c a) (HashMap d b) a b where

-- | @'each' :: 'Traversal' ('Vector.Vector' a) ('Vector.Vector' b) a b@
instance Each (Vector.Vector a) (Vector.Vector b) a b where
  each = noIx vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Prim' a, 'Prim' b) => 'Traversal' ('Prim.Vector' a)
-- ('Prim.Vector' b) a b@
instance (Prim a, Prim b) => Each (Prim.Vector a) (Prim.Vector b) a b where
  each = noIx vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Storable' a, 'Storable' b) => 'Traversal' ('Storable.Vector'
-- a) ('Storable.Vector' b) a b@
instance (Storable a, Storable b) => Each (Storable.Vector a) (Storable.Vector b) a b where
  each = noIx vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Unbox' a, 'Unbox' b) => 'Traversal' ('Unboxed.Vector' a)
-- ('Unboxed.Vector' b) a b@
instance (Unbox a, Unbox b) => Each (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each = noIx vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'StrictT.Text' 'StrictT.Text' 'Char' 'Char'@
instance (a ~ Char, b ~ Char) => Each ST.Text ST.Text a b where
  each = noIx text
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'LazyT.Text' 'LazyT.Text' 'Char' 'Char'@
instance (a ~ Char, b ~ Char) => Each LT.Text LT.Text a b where
  each = noIx text
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'StrictB.ByteString' 'StrictB.ByteString' 'Word8'
-- 'Word8'@
instance (a ~ Word8, b ~ Word8) => Each SB.ByteString SB.ByteString a b where
  each = noIx traversedStrictTree
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'LazyB.ByteString' 'LazyB.ByteString' 'Word8'
-- 'Word8'@
instance (a ~ Word8, b ~ Word8) => Each LB.ByteString LB.ByteString a b where
  each = noIx traversedLazy
  {-# INLINE each #-}

-- $setup
-- >>> import Optics
-- >>> import Optics.Operators
