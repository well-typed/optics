module Optics.Internal.Iso where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying iso.
type Iso s t a b = Optic An_Iso NoIx s t a b

-- | Type synonym for a type-preserving iso.
type Iso' s a = Optic' An_Iso NoIx s a

-- | Explicitly cast an optic to an iso.
toIso :: Is k An_Iso => Optic k is s t a b -> Optic An_Iso is s t a b
toIso = castOptic
{-# INLINE toIso #-}

-- | Build an iso from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (dimap f g)
{-# INLINE iso #-}

-- | Extract the two components of an isomorphism.
withIso :: Is k An_Iso => Optic k is s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o k = case getOptic (toIso o) (Exchange id id) of
  Exchange sa bt -> k sa bt
{-# INLINE withIso #-}

----------------------------------------

-- | Type to represent the components of an isomorphism.
data Exchange a b i s t =
  Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap ss tt (Exchange sa bt) = Exchange (sa . ss) (tt . bt)
  {-# INLINE dimap #-}
