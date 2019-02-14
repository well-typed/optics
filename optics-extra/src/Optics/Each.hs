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
import Data.Int
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
import Optics.Indexed ()
import Optics.Extra.Internal.ByteString

-- Extra instances

-- | @'each' :: 'IxTraversal' k ('HashMap' k a) ('HashMap' k b) a b@
instance k ~ k' => Each k (HashMap k a) (HashMap k' b) a b where

-- | @'each' :: 'IxTraversal' Int ('Vector.Vector' a) ('Vector.Vector' b) a b@
instance i ~ Int => Each i (Vector.Vector a) (Vector.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Prim' a, 'Prim' b) => 'IxTraversal' Int ('Prim.Vector' a)
-- ('Prim.Vector' b) a b@
instance
  (Prim a, Prim b, i ~ Int
  ) => Each i (Prim.Vector a) (Prim.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Storable' a, 'Storable' b) => 'Traversal' Int ('Storable.Vector'
-- a) ('Storable.Vector' b) a b@
instance
  (Storable a, Storable b, i ~ Int
  ) => Each i (Storable.Vector a) (Storable.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: ('Unbox' a, 'Unbox' b) => 'IxTraversal' Int ('Unboxed.Vector' a)
-- ('Unboxed.Vector' b) a b@
instance
  (Unbox a, Unbox b, i ~ Int
  ) => Each i (Unboxed.Vector a) (Unboxed.Vector b) a b where
  each = vectorTraverse
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'StrictT.Text' 'StrictT.Text' 'Char' 'Char'@
instance
  (a ~ Char, b ~ Char, i ~ Int
  ) => Each i ST.Text ST.Text a b where
  each = text
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'LazyT.Text' 'LazyT.Text' 'Char' 'Char'@
instance
  (a ~ Char, b ~ Char, i ~ Int
  ) => Each i LT.Text LT.Text a b where
  each = text
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' 'StrictB.ByteString' 'StrictB.ByteString' 'Word8'
-- 'Word8'@
instance
  (a ~ Word8, b ~ Word8, i ~ Int
  ) => Each i SB.ByteString SB.ByteString a b where
  each = traversedStrictTree
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int64' 'LazyB.ByteString' 'LazyB.ByteString'
-- 'Word8' 'Word8'@
instance
  (a ~ Word8, b ~ Word8, i ~ Int64
  ) => Each i LB.ByteString LB.ByteString a b where
  each = traversedLazy
  {-# INLINE each #-}
