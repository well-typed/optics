{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Iso where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for an iso.
data An_Iso

-- | Constraints corresponding to an iso.
type instance Constraints An_Iso p = Profunctor p

-- | Type synonym for a type-modifying iso.
type Iso s t a b = Optic An_Iso s t a b

-- | Type synonym for a type-preserving iso.
type Iso' s a = Optic' An_Iso s a

-- | Explicitly cast an optic to an iso.
toIso :: Is k An_Iso => Optic k s t a b -> Iso s t a b
toIso = sub
{-# INLINE toIso #-}

-- | Build an iso from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (dimap f g)
{-# INLINE iso #-}

-- | Type to represent the components of an isomorphism.
data Exchange a b s t =
  Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap tt (Exchange sa bt) = Exchange sa (tt . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap ss tt (Exchange sa bt) = Exchange (sa . ss) (tt . bt)
  {-# INLINE dimap #-}

-- | Extract the two components of an isomorphism.
withIso :: Is k An_Iso => Optic k s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o k = case getOptic (toIso o) (Exchange id id) of
  Exchange sa bt -> k sa bt
{-# INLINE withIso #-}
