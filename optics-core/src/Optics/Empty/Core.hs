-- |
-- Module: Optics.Empty.Core
-- Description: A 'Prism' for a type that may be '_Empty'.
--
-- This module defines the 'AsEmpty' class, which provides a 'Prism' for a type
-- that may be '_Empty'.
--
-- Note that orphan instances for this class are defined in the @Optics.Empty@
-- module from @optics-extra@, so if you are not simply depending on @optics@
-- you may wish to import that module instead.
--
-- >>> isn't _Empty [1,2,3]
-- True
--
-- >>> case Nothing of { Empty -> True; _ -> False }
-- True
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Optics.Empty.Core
  ( AsEmpty(..)
  , pattern Empty
  ) where

import Control.Applicative (ZipList(..))
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set as Set
import qualified Data.Sequence as Seq

import Data.Profunctor.Indexed

import Data.Maybe.Optics
import Optics.AffineTraversal
import Optics.Fold
import Optics.Iso
import Optics.Optic
import Optics.Prism
import Optics.Review

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS)
import GHC.Event
#endif

-- | Class for types that may be '_Empty'.
--
class AsEmpty a where
  -- |
  --
  -- >>> isn't _Empty [1,2,3]
  -- True
  _Empty :: Prism' a ()
  default _Empty :: (Monoid a, Eq a) => Prism' a ()
  _Empty = only mempty
  {-# INLINE _Empty #-}

-- | Pattern synonym for matching on any type with an 'AsEmpty' instance.
--
-- >>> case Nothing of { Empty -> True; _ -> False }
-- True
--
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

instance AsEmpty (IntMap a) where
  _Empty = nearly IntMap.empty IntMap.null
  {-# INLINE _Empty #-}

instance AsEmpty (Set a) where
  _Empty = nearly Set.empty Set.null
  {-# INLINE _Empty #-}

instance AsEmpty IntSet where
  _Empty = nearly IntSet.empty IntSet.null
  {-# INLINE _Empty #-}

instance AsEmpty (Seq.Seq a) where
  _Empty = nearly Seq.empty Seq.null
  {-# INLINE _Empty #-}

-- $setup
-- >>> import Optics.Core
