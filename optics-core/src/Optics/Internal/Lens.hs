{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Lens where

import Optics.Internal.Optic

-- | Tag for a lens.
data A_Lens

-- | Constraints corresponding to a lens.
type instance Constraints A_Lens p f = (p ~ (->), Functor f)

-- | Type synonym for a type-modifying lens.
type Lens s t a b = Optic A_Lens s t a b

-- | Type synonym for a type-preserving lens.
type Lens' s a = Optic' A_Lens s a

-- | Explicitly cast an optic to a lens.
toLens :: Is k A_Lens => Optic k s t a b -> Lens s t a b
toLens = sub
{-# INLINE toLens #-}

-- | Build a lens from the van Laarhoven representation.
vlLens :: (forall f . Functor f => (a -> f b) -> s -> f t) -> Lens s t a b
vlLens = Optic
{-# INLINE vlLens #-}

-- | Build a lens from a getter and setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = vlLens (\ f s -> set s <$> f (get s))
{-# INLINE lens #-}
