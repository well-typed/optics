module Optics.Internal.Iso where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying iso.
type Iso i s t a b = Optic An_Iso i i s t a b

-- | Type synonym for a type-preserving iso.
type Iso' i s a = Optic' An_Iso i i s a

-- | Explicitly cast an optic to an iso.
toIso :: Is k An_Iso => Optic k i i s t a b -> Iso i s t a b
toIso = sub
{-# INLINE toIso #-}

-- | Build an iso from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso i s t a b
iso f g = Optic (dimap f g)
{-# INLINE iso #-}

-- | Extract the two components of an isomorphism.
withIso :: Is k An_Iso => Optic k i i s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o k = case getOptic (toIso o) (Exchange id id) of
  Exchange sa bt -> k sa bt
{-# INLINE withIso #-}

----------------------------------------

-- | Type to represent the components of an isomorphism.
data Exchange a b i s t =
  Exchange (s -> a) (b -> t)

instance Functor (Exchange a b i s) where
  fmap tt (Exchange sa bt) = Exchange sa (tt . bt)
  {-# INLINE fmap #-}

instance Profunctor (Exchange a b) where
  dimap ss tt (Exchange sa bt) = Exchange (sa . ss) (tt . bt)
  {-# INLINE dimap #-}
