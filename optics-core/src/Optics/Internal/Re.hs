module Optics.Internal.Re where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

class ReversibleOptic k where
  type ReversedOptic k :: *
  -- | Reverses optics, turning around 'Equality' into 'Equality', 'Iso' into
  -- 'Iso', 'Prism' into 'PrismaticGetter' (and back), 'Lens' into 'LensyReview'
  -- (and back) and 'Getter' into 'Review' (and back).
  re :: Optic k '[] s t a b -> Optic (ReversedOptic k) '[] b a t s

instance ReversibleOptic An_Equality where
  type ReversedOptic An_Equality = An_Equality
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic An_Iso where
  type ReversedOptic An_Iso = An_Iso
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_Prism where
  type ReversedOptic A_Prism = A_PrismaticGetter
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_PrismaticGetter where
  type ReversedOptic A_PrismaticGetter = A_Prism
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_Lens where
  type ReversedOptic A_Lens = A_LensyReview
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_LensyReview where
  type ReversedOptic A_LensyReview = A_Lens
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_Getter where
  type ReversedOptic A_Getter = A_Review
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_Review where
  type ReversedOptic A_Review = A_Getter
  re o = Optic (re__ o)
  {-# INLINE re #-}

-- | Internal implementation of re.
re__
  :: Constraints k (Re p a b)
  => Optic k   '[] s t a b
  -> Optic__ p i i b a t s
re__ o = unRe (getOptic o (Re id))
{-# INLINE re__ #-}

----------------------------------------

-- | Helper for reversing optics.
newtype Re p s t i a b = Re { unRe :: p i b a -> p i t s }

instance Profunctor p => Profunctor (Re p s t) where
  dimap f g (Re p) = Re (p . dimap g f)
  {-# INLINE dimap #-}

instance Bicontravariant p => Bifunctor (Re p s t) where
  bimap  f g (Re p) = Re (p . contrabimap g f)
  first  f   (Re p) = Re (p . contrasecond f)
  second   g (Re p) = Re (p . contrafirst g)
  {-# INLINE bimap #-}
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifunctor p => Bicontravariant (Re p s t) where
  contrabimap  f g (Re p) = Re (p . bimap g f)
  contrafirst  f   (Re p) = Re (p . second f)
  contrasecond   g (Re p) = Re (p . first g)
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Strong p => Costrong (Re p s t) where
  unfirst  (Re p) = Re (p . first')
  unsecond (Re p) = Re (p . second')
  {-# INLINE unfirst #-}
  {-# INLINE unsecond #-}

instance Costrong p => Strong (Re p s t) where
  first'  (Re p) = Re (p . unfirst)
  second' (Re p) = Re (p . unsecond)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Choice p => Cochoice (Re p s t) where
  unleft  (Re p) = Re (p . left')
  unright (Re p) = Re (p . right')
  {-# INLINE unleft #-}
  {-# INLINE unright #-}

instance Cochoice p => Choice (Re p s t) where
  left'  (Re p) = Re (p . unleft)
  right' (Re p) = Re (p . unright)
  {-# INLINE left' #-}
  {-# INLINE right' #-}
