module Optics.Internal.Generic.Utils where

import Data.Bifunctor
import GHC.Generics
import GHC.Generics.Optics
import Optics.Internal.Optic
import Optics.Iso
import Optics.Lens
import Optics.Prism

-- | Convert from generic to concrete representation. We use 'withLens' to make
-- the choice of profunctor known so that GHC optimizes away generic code and
-- we're left with the equivalent of hand-written lens.
genericLens
  :: (Generic s, Generic t)
  => Lens (Rep s c) (Rep t c) a b
  -> Lens s t a b
genericLens o = withLens (generic % o) lens
{-# INLINE genericLens #-}

-- | Convert from generic to concrete representation. We use 'withPrism' to make
-- the choice of profunctor known so that GHC optimizes away generic code and
-- we're left with the equivalent of hand-written prism.
genericPrism
  :: (Generic s, Generic t)
  => Prism (Rep s c) (Rep t c) a b
  -> Prism s t a b
genericPrism o = withPrism (generic % o) prism
{-# INLINE genericPrism #-}

assoc3 :: Iso ((a, b), c) ((a', b'), c') (a, (b, c)) (a', (b', c'))
assoc3 = iso (\((a, b), c) -> (a, (b, c))) (\(a, (b, c)) -> ((a, b), c))
{-# INLINE assoc3 #-}

-- | Lens focusing on the first element of a product
gfirst :: Lens ((a :*: c) t) ((b :*: c) t) (a t) (b t)
gfirst = lens (\(a :*: _) -> a) (\(_ :*: c) b -> b :*: c)
{-# INLINE gfirst #-}

-- | Lens focusing on the second element of a product
gsecond :: Lens ((c :*: a) t) ((c :*: b) t) (a t) (b t)
gsecond = lens (\(_ :*: a) -> a) (\(c :*: _) b -> c :*: b)
{-# INLINE gsecond #-}

gproduct :: Iso ((a :*: b) t) ((a' :*: b') t) (a t, b t) (a' t, b' t)
gproduct = iso (\(a :*: b) -> (a, b)) (\(a, b) -> (a :*: b))
{-# INLINE gproduct #-}

gchoosing
  :: Lens (a t) (a' t) c d
  -> Lens (b t) (b' t) c d
  -> Lens ((a :+: b) t) ((a' :+: b') t) c d
gchoosing l r = lensVL $ \f -> \case
  L1 v -> L1 <$> toLensVL l f v
  R1 v -> R1 <$> toLensVL r f v
{-# INLINE gchoosing #-}

pairing
  :: (Is k An_Iso, Is l An_Iso)
  => Optic k is s  t  a  b
  -> Optic l js s' t' a' b'
  -> Iso (s, s') (t, t') (a, a') (b, b')
pairing f g = withIso f $ \sa bt -> withIso g $ \s'a' b't' ->
  iso (bimap sa s'a') (bimap bt b't')
{-# INLINE pairing #-}
