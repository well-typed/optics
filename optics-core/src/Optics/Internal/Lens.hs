module Optics.Internal.Lens where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a type-modifying lens.
type Lens i s t a b = Optic A_Lens i i s t a b

-- | Type synonym for a type-preserving lens.
type Lens' i s a = Optic' A_Lens i i s a

-- | Explicitly cast an optic to a lens.
toLens :: Is k A_Lens => Optic k i i s t a b -> Lens i s t a b
toLens = sub
{-# INLINE toLens #-}

-- | Build a lens from the van Laarhoven representation.
vlLens :: (forall f. Functor f => (a -> f b) -> s -> f t)
       -> Lens i s t a b
vlLens l = Optic $
  dimap ((\(Context f a) -> (f, a)) . l (Context id))
        (\(f, b) -> f b)
  . second' -- p (b -> t, a) (b -> t, b)
{-# INLINE vlLens #-}

-- | Build a lens from a getter and setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens i s t a b
lens get set = Optic $
  dimap (\s -> (get s, s))
        (\(b, s) -> set s b)
  . first'
{-# INLINE lens #-}

-- | Work with a lens in van Laarhoven representation.
withLens
  :: Is k A_Lens
  => Optic k i i s t a b
  -> ((forall f. Functor f => (a -> f b) -> s -> f t) -> r)
  -> r
withLens o k = k (runStar #. getOptic (toLens o) .# Star)
{-# INLINE withLens #-}
