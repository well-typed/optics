module Optics.Internal.Lens where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a type-modifying lens.
type Lens i s t a b = Optic A_Lens i i s t a b

-- | Type synonym for a type-preserving lens.
type Lens' i s a = Optic' A_Lens i i s a

-- | Type synonym for a type-modifying van Laarhoven lens.
type LensVL s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven lens.
type LensVL' s a = LensVL s s a a

-- | Explicitly cast an optic to a lens.
toLens :: Is k A_Lens => Optic k i i s t a b -> Lens i s t a b
toLens = sub
{-# INLINE toLens #-}

-- | Build a lens from a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens i s t a b
lens get set = Optic $
  dimap (\s -> (get s, s))
        (\(b, s) -> set s b)
  . first'
{-# INLINE lens #-}

-- | Work with a lens as a getter and a setter.
withLens
  :: Is k A_Lens
  => Optic k i i s t a b
  -> ((s -> a) -> (s -> b -> t) -> r)
  -> r
withLens o k = case getOptic (toLens o) $ Store id (\_ -> id) of
  Store get set -> k get set
{-# INLINE withLens #-}

-- | Build a lens from the van Laarhoven representation.
lensVL :: LensVL s t a b -> Lens i s t a b
lensVL l = Optic $
  dimap ((\(Context f a) -> (f, a)) . l (Context id))
        (\(f, b) -> f b)
  . second' -- p (b -> t, a) (b -> t, b)
{-# INLINE lensVL #-}

-- | Convert a lens to the van Laarhoven representation.
toLensVL :: Is k A_Lens => Optic k i i s t a b -> LensVL s t a b
toLensVL o = runStar #. getOptic (toLens o) .# Star
{-# INLINE toLensVL #-}

-- | Work with a lens in the van Laarhoven representation.
withLensVL
  :: Is k A_Lens
  => Optic k i i s t a b
  -> (LensVL s t a b -> r)
  -> r
withLensVL o k = k (toLensVL o)
{-# INLINE withLensVL #-}

----------------------------------------

-- | Type to represent the components of a lens.
data Store a b i s t = Store (s -> a) (s -> b -> t)

instance Profunctor (Store a b) where
  dimap f g (Store get set) = Store (get . f) (\s -> g . set (f s))
  {-# INLINE dimap #-}

instance Strong (Store a b) where
  first' (Store get set) = Store (get . fst) (\(s, c) b -> (set s b, c))
  second' (Store get set) = Store (get . snd) (\(c, s) b -> (c, set s b))
  {-# INLINE first' #-}
  {-# INLINE second' #-}
