module Optics.Internal.Lens where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a type-modifying lens.
type Lens s t a b = Optic A_Lens s t a b

-- | Type synonym for a type-preserving lens.
type Lens' s a = Optic' A_Lens s a

-- | Explicitly cast an optic to a lens.
toLens :: Is k A_Lens => Optic k s t a b -> Lens s t a b
toLens = sub
{-# INLINE toLens #-}

-- | Build a lens from the van Laarhoven representation.
vlLens :: (forall f. Functor f => (a -> f b) -> s -> f t)
       -> Lens s t a b
vlLens l = Optic $
  dimap ((\(Context f a) -> (f, a)) . l (Context id))
        (\(f, b) -> f b)
  . second' -- p (b -> t, a) (b -> t, b)
{-# INLINE vlLens #-}

-- | Build a lens from a getter and setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = Optic $
  dimap (\s -> (get s, s))
        (\(b, s) -> set s b)
  . first'
{-# INLINE lens #-}
