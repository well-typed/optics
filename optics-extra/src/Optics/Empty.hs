-- |
-- Module: Optics.Empty
-- Description: A 'Prism' for a type that may be '_Empty'.
--
-- This module defines the 'AsEmpty' class, which provides a 'Prism' for a type
-- that may be '_Empty'.
--
-- >>> isn't _Empty [1,2,3]
-- True
--
-- >>> case Nothing of { Empty -> True; _ -> False }
-- True
--
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Optics.Empty
  ( AsEmpty(..)
  , pattern Empty
  ) where

import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Vector as Vector
import Data.Vector.Storable as Storable
import Data.Vector.Unboxed as Unboxed

import Optics.Core

-- Extra instances

instance AsEmpty (HashMap k a) where
  _Empty = nearly HashMap.empty HashMap.null
  {-# INLINE _Empty #-}

instance AsEmpty (HashSet a) where
  _Empty = nearly HashSet.empty HashSet.null
  {-# INLINE _Empty #-}

instance AsEmpty (Vector.Vector a) where
  _Empty = nearly Vector.empty Vector.null
  {-# INLINE _Empty #-}

instance Unbox a => AsEmpty (Unboxed.Vector a) where
  _Empty = nearly Unboxed.empty Unboxed.null
  {-# INLINE _Empty #-}

instance Storable a => AsEmpty (Storable.Vector a) where
  _Empty = nearly Storable.empty Storable.null
  {-# INLINE _Empty #-}

instance AsEmpty StrictB.ByteString where
  _Empty = nearly StrictB.empty StrictB.null
  {-# INLINE _Empty #-}

instance AsEmpty LazyB.ByteString where
  _Empty = nearly LazyB.empty LazyB.null
  {-# INLINE _Empty #-}

instance AsEmpty StrictT.Text where
  _Empty = nearly StrictT.empty StrictT.null
  {-# INLINE _Empty #-}

instance AsEmpty LazyT.Text where
  _Empty = nearly LazyT.empty LazyT.null
  {-# INLINE _Empty #-}
