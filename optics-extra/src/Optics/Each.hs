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
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Optics.Core
import Optics.Indexed ()
import Optics.Extra.Internal.ByteString

-- Extra instances

-- | @'each' :: 'IxTraversal' k ('HashMap' k a) ('HashMap' k b) a b@
instance k ~ k' => Each k (HashMap k a) (HashMap k' b) a b where
  -- traverseWithKey has best performance for all flavours for some reason.
  each = ixTraversalVL HashMap.traverseWithKey
  {-# INLINE[1] each #-}

-- | @'each' :: 'IxTraversal' Int ('V.Vector' a) ('V.Vector' b) a b@
instance Each Int (V.Vector a) (V.Vector b) a b where
  each = vectorTraverse
  {-# INLINE[1] each #-}

-- | @'each' :: ('Prim' a, 'Prim' b) => 'IxTraversal' Int ('Prim.Vector' a)
-- ('Prim.Vector' b) a b@
instance (Prim a, Prim b) => Each Int (VP.Vector a) (VP.Vector b) a b where
  each = vectorTraverse
  {-# INLINE[1] each #-}

-- | @'each' :: ('Storable' a, 'Storable' b) => 'IxTraversal' 'Int' ('VS.Vector'
-- a) ('VS.Vector' b) a b@
instance (Storable a, Storable b) => Each Int (VS.Vector a) (VS.Vector b) a b where
  each = vectorTraverse
  {-# INLINE[1] each #-}

-- | @'each' :: ('Unbox' a, 'Unbox' b) => 'IxTraversal' 'Int' ('VU.Vector' a)
-- ('VU.Vector' b) a b@
instance (Unbox a, Unbox b ) => Each Int (VU.Vector a) (VU.Vector b) a b where
  each = vectorTraverse
  {-# INLINE[1] each #-}

-- | @'each' :: 'IxTraversal' 'Int' 'ST.Text' 'ST.Text' 'Char' 'Char'@
instance (a ~ Char, b ~ Char) => Each Int ST.Text ST.Text a b where
  each = text
  {-# INLINE[1] each #-}

-- | @'each' :: 'IxTraversal' 'Int64' 'LT.Text' 'LT.Text' 'Char' 'Char'@
instance (a ~ Char, b ~ Char) => Each Int LT.Text LT.Text a b where
  each = text
  {-# INLINE[1] each #-}

-- | @'each' :: 'IxTraversal' 'Int' 'SB.ByteString' 'SB.ByteString' 'Word8'
-- 'Word8'@
instance (a ~ Word8, b ~ Word8) => Each Int64 SB.ByteString SB.ByteString a b where
  each = traversedStrictTree
  {-# INLINE[1] each #-}

-- | @'each' :: 'IxTraversal' 'Int64' 'LB.ByteString' 'LB.ByteString' 'Word8'
-- 'Word8'@
instance (a ~ Word8, b ~ Word8) => Each Int64 LB.ByteString LB.ByteString a b where
  each = traversedLazy
  {-# INLINE[1] each #-}
