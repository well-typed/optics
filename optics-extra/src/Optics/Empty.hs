{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Optics.Empty
  ( AsEmpty(..)
  , pattern Empty
  ) where

import Control.Applicative (ZipList(..))
import Data.ByteString as StrictB
import Data.ByteString.Lazy as LazyB
import Data.HashMap.Lazy as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set as Set
import Data.Text as StrictT
import Data.Text.Lazy as LazyT
import Data.Vector as Vector
import Data.Vector.Storable as Storable
import Data.Vector.Unboxed as Unboxed
import qualified Data.Sequence as Seq

import Optics.Core
import Optics.Internal.Utils

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import GHC.Event
#endif

class AsEmpty a where
  -- |
  --
  -- >>> isn't _Empty [1,2,3]
  -- True
  _Empty :: Prism' a ()
  default _Empty :: (Monoid a, Eq a) => Prism' a ()
  _Empty = only mempty
  {-# INLINE _Empty #-}

pattern Empty :: forall a. AsEmpty a => a
pattern Empty <- (has _Empty -> True) where
  Empty = review _Empty ()

{- Default Monoid instances -}
instance AsEmpty Ordering
instance AsEmpty ()
instance AsEmpty Any
instance AsEmpty All
#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
instance AsEmpty Event
#endif
instance (Eq a, Num a) => AsEmpty (Product a)
instance (Eq a, Num a) => AsEmpty (Sum a)

instance AsEmpty (Maybe a) where
  _Empty = _Nothing
  {-# INLINE _Empty #-}

instance AsEmpty (Last a) where
  _Empty = nearly (Last Nothing) (isNothing .# getLast)
  {-# INLINE _Empty #-}

instance AsEmpty (First a) where
  _Empty = nearly (First Nothing) (isNothing .# getFirst)
  {-# INLINE _Empty #-}

instance AsEmpty a => AsEmpty (Dual a) where
  _Empty = iso getDual Dual % _Empty
  {-# INLINE _Empty #-}

instance (AsEmpty a, AsEmpty b) => AsEmpty (a, b) where
  _Empty = prism'
    (\() -> (review _Empty (), review _Empty ()))
    (\(s, s') -> case matching _Empty s of
        Right () -> case matching _Empty s' of
          Right () -> Just ()
          Left _   -> Nothing
        Left _   -> Nothing)
  {-# INLINE _Empty #-}

instance (AsEmpty a, AsEmpty b, AsEmpty c) => AsEmpty (a, b, c) where
  _Empty = prism'
    (\() -> (review _Empty (), review _Empty (), review _Empty ()))
    (\(s, s', s'') -> case matching _Empty s of
        Right () -> case matching _Empty s' of
          Right () -> case matching _Empty s'' of
            Right () -> Just ()
            Left _   -> Nothing
          Left _   -> Nothing
        Left _   -> Nothing)
  {-# INLINE _Empty #-}

instance AsEmpty [a] where
  _Empty = nearly [] Prelude.null
  {-# INLINE _Empty #-}

instance AsEmpty (ZipList a) where
  _Empty = nearly (ZipList []) (Prelude.null . getZipList)
  {-# INLINE _Empty #-}

instance AsEmpty (Map k a) where
  _Empty = nearly Map.empty Map.null
  {-# INLINE _Empty #-}

instance AsEmpty (HashMap k a) where
  _Empty = nearly HashMap.empty HashMap.null
  {-# INLINE _Empty #-}

instance AsEmpty (IntMap a) where
  _Empty = nearly IntMap.empty IntMap.null
  {-# INLINE _Empty #-}

instance AsEmpty (Set a) where
  _Empty = nearly Set.empty Set.null
  {-# INLINE _Empty #-}

instance AsEmpty (HashSet a) where
  _Empty = nearly HashSet.empty HashSet.null
  {-# INLINE _Empty #-}

instance AsEmpty IntSet where
  _Empty = nearly IntSet.empty IntSet.null
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

instance AsEmpty (Seq.Seq a) where
  _Empty = nearly Seq.empty Seq.null
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
