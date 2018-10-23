module Optics.Internal.Re where

import Data.Bifunctor

import Optics.Internal.Bicontravariant
import Optics.Internal.Optic
import Optics.Internal.Profunctor

class ReversibleOptic k where
  type ReversedOptic k :: *
  -- | Reverses optics, turning around 'Iso' into 'Iso', 'Prism' into 'Getter',
  -- 'Lens' into 'Review' and 'Getter' into 'Review' (and back).
  re :: Optic k s t a b -> Optic (ReversedOptic k) b a t s

instance ReversibleOptic An_Iso where
  type ReversedOptic An_Iso = An_Iso
  re o = Optic (re__ (getOptic o))
  {-# INLINE re #-}

instance ReversibleOptic A_Prism where
  type ReversedOptic A_Prism = A_Getter
  re o = Optic (re__ (getOptic o))
  {-# INLINE re #-}

instance ReversibleOptic A_Lens where
  type ReversedOptic A_Lens = A_Review
  re o = Optic (re__ (getOptic o))
  {-# INLINE re #-}

instance ReversibleOptic A_Getter where
  type ReversedOptic A_Getter = A_Review
  re o = Optic (re__ (getOptic o))
  {-# INLINE re #-}

instance ReversibleOptic A_Review where
  type ReversedOptic A_Review = A_Getter
  re o = Optic (re__ (getOptic o))
  {-# INLINE re #-}

-- | Internal implementation of re.
re__ :: Optic__ (Re p a b) s t a b -> Optic__ p b a t s
re__ o = unRe (o (Re id))
{-# INLINE re__ #-}

----------------------------------------

-- | Helper for reversing optics.
newtype Re p s t a b = Re { unRe :: p b a -> p t s }

instance Profunctor p => Profunctor (Re p s t) where
  dimap f g (Re p) = Re (p . dimap g f)
  {-# INLINE dimap #-}

instance Bicontravariant p => Bifunctor (Re p s t) where
  bimap f g (Re p) = Re (p . contrabimap g f)
  {-# INLINE bimap #-}

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
