{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Iso where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for an iso.
data An_Iso

-- | Constraints corresponding to an iso.
type instance Constraints An_Iso p f = (Profunctor p, Functor f)

-- | Type synonym for a type-modifying iso.
type Iso s t a b = Optic An_Iso s t a b

-- | Type synonym for a type-preserving iso.
type Iso' s a = Optic' An_Iso s a

-- | Explicitly cast an optic to an iso.
toIso :: Is k An_Iso => Optic k s t a b -> Iso s t a b
toIso = sub
{-# INLINE toIso #-}

-- | Create a lens.
mkIso :: Optic_ An_Iso s t a b -> Iso s t a b
mkIso = Optic
{-# INLINE mkIso #-}

-- | Build an iso from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (dimap f (fmap g))
{-# INLINE iso #-}
