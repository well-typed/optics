{-# LANGUAGE TypeFamilyDependencies #-}
-- |
-- Module: Optics.Re
-- Description: The 're' operator allows some optics to be reversed.
--
-- Some optics can be reversed with 're'.  This is mainly useful to invert
-- 'Optics.Iso.Iso's:
--
-- >>> let _Identity = iso runIdentity Identity
-- >>> view (_1 % re _Identity) ('x', "yz")
-- Identity 'x'
--
-- Yet we can use a 'Optics.Lens.Lens' as a 'Optics.Review.Review' too:
--
-- >>> review (re _1) ('x', "yz")
-- 'x'
--
-- In the following diagram, red arrows illustrate how 're' transforms optics.
-- The 'Optics.ReversedLens.ReversedLens' and
-- 'Optics.ReversedPrism.ReversedPrism' optic kinds are backwards versions of
-- 'Optics.Lens.Lens' and 'Optics.Prism.Prism' respectively, and are present so
-- that @'re' . 're'@ does not change the optic kind.
--
-- <<diagrams/reoptics.png Reversed Optics>>
--
module Optics.Re
  ( ReversibleOptic(..)
  ) where

import Data.Coerce

import Data.Profunctor.Indexed

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Optic

-- | Class for optics that can be 're'versed.
class ReversibleOptic k where
  -- | Injective type family that maps an optic kind to the optic kind produced
  -- by 're'versing it.
  --
  -- @
  -- 'ReversedOptic' 'An_Iso'            = 'An_Iso'
  -- 'ReversedOptic' 'A_Prism'           = 'A_ReversedPrism'
  -- 'ReversedOptic' 'A_ReversedPrism'   = 'A_Prism'
  -- 'ReversedOptic' 'A_Lens'            = 'A_ReversedLens'
  -- 'ReversedOptic' 'A_ReversedLens'    = 'A_Lens'
  -- 'ReversedOptic' 'A_Getter'          = 'A_Review'
  -- 'ReversedOptic' 'A_Review'          = 'A_Getter'
  -- @
  type ReversedOptic k = r | r -> k
  -- | Reverses optics, turning around 'Optics.Iso.Iso' into 'Optics.Iso.Iso',
  -- 'Optics.Prism.Prism' into 'Optics.ReversedPrism.ReversedPrism' (and
  -- back), 'Optics.Lens.Lens' into 'Optics.ReversedLens.ReversedLens' (and back)
  -- and 'Optics.Getter.Getter' into 'Optics.Review.Review' (and back).
  re
    :: "re" `AcceptsEmptyIndices` is
    => Optic                k  is s t a b
    -> Optic (ReversedOptic k) is b a t s

instance ReversibleOptic An_Iso where
  type ReversedOptic An_Iso = An_Iso
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_Prism where
  type ReversedOptic A_Prism = A_ReversedPrism
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_ReversedPrism where
  type ReversedOptic A_ReversedPrism = A_Prism
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_Lens where
  type ReversedOptic A_Lens = A_ReversedLens
  re o = Optic (re__ o)
  {-# INLINE re #-}

instance ReversibleOptic A_ReversedLens where
  type ReversedOptic A_ReversedLens = A_Lens
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
  :: (Profunctor p, Constraints k (Re p a b))
  => Optic k  NoIx s t a b
  -> Optic__ p i i b a t s
re__ o = unRe (getOptic o (Re id))
{-# INLINE re__ #-}

----------------------------------------

-- | Helper for reversing optics.
newtype Re p s t i a b = Re { unRe :: p i b a -> p i t s }

instance Profunctor p => Profunctor (Re p s t) where
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

  conjoined__ = error "conjoined__(Re) shouldn't be reachable"
  ixcontramap = error "ixcontramap(Re) shouldn't be reachable"

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

  ilinear _ = error "ilinear(Re) shouldn't be reachable"

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

-- $setup
-- >>> import Data.Functor.Identity
-- >>> import Optics.Core
