{-# LANGUAGE TypeFamilyDependencies #-}
module Optics.Re
  ( ReversibleOptic(..)
  ) where

import Data.Coerce

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

class ReversibleOptic k where
  type ReversedOptic k = r | r -> k
  -- | Reverses optics, turning around 'Optics.Iso.Iso' into 'Optics.Iso.Iso',
  -- 'Optics.Prism.Prism' into 'Optics.PrismaticGetter.PrismaticGetter' (and
  -- back), 'Optics.Lens.Lens' into 'Optics.LensyReview.LensyReview' (and back)
  -- and 'Optics.Getter.Getter' into 'Optics.Review.Review' (and back).
  re :: Optic k NoIx s t a b -> Optic (ReversedOptic k) NoIx b a t s

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
  :: (Profunctor p, Constraints k (Re p a b i ci))
  => Optic k        NoIx s t a b
  -> Optic__ p i ci i ci b a t s
re__ o = unRe (getOptic o (Re id))
{-# INLINE re__ #-}

----------------------------------------

-- | Helper for reversing optics.
newtype Re p s t j cj ci i a b = Re { unRe :: p i ci b a -> p j cj t s }

instance Profunctor p => Profunctor (Re p s t j cj) where
  dimap f g (Re p) = Re (p . dimap g f)
  lmap  f   (Re p) = Re (p . rmap f)
  rmap    g (Re p) = Re (p . lmap g)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  lcoerce' = lmap coerce
  rcoerce' = rmap coerce
  {-# INLINE lcoerce' #-}
  {-# INLINE rcoerce' #-}

  conjoined__ _ f = f
  {-# INLINE conjoined__ #-}

  ixcontramap ij (Re f) = Re (f . ixmap ij)
  ixmap       ij (Re f) = Re (f . ixcontramap ij)
  {-# INLINE ixcontramap #-}
  {-# INLINE ixmap #-}

instance Bicontravariant p => Bifunctor (Re p s t j cj) where
  bimap  f g (Re p) = Re (p . contrabimap g f)
  first  f   (Re p) = Re (p . contrasecond f)
  second   g (Re p) = Re (p . contrafirst g)
  {-# INLINE bimap #-}
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Bifunctor p => Bicontravariant (Re p s t j cj) where
  contrabimap  f g (Re p) = Re (p . bimap g f)
  contrafirst  f   (Re p) = Re (p . second f)
  contrasecond   g (Re p) = Re (p . first g)
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Strong p => Costrong (Re p s t j cj) where
  unfirst  (Re p) = Re (p . first')
  unsecond (Re p) = Re (p . second')
  {-# INLINE unfirst #-}
  {-# INLINE unsecond #-}

  colinear  f (Re p) = Re (p . linear f)
  icolinear f (Re p) = Re (p . ilinear f)
  {-# INLINE colinear #-}
  {-# INLINE icolinear #-}

instance Costrong p => Strong (Re p s t j cj) where
  first'  (Re p) = Re (p . unfirst)
  second' (Re p) = Re (p . unsecond)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear  f (Re p) = Re (p . colinear f)
  ilinear f (Re p) = Re (p . icolinear f)
  {-# INLINE linear #-}
  {-# INLINE ilinear #-}

instance Choice p => Cochoice (Re p s t j cj) where
  unleft  (Re p) = Re (p . left')
  unright (Re p) = Re (p . right')
  {-# INLINE unleft #-}
  {-# INLINE unright #-}

instance Cochoice p => Choice (Re p s t j cj) where
  left'  (Re p) = Re (p . unleft)
  right' (Re p) = Re (p . unright)
  {-# INLINE left' #-}
  {-# INLINE right' #-}
