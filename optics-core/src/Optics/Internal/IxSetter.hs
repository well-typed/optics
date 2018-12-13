module Optics.Internal.IxSetter where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying indexed setter.
type IxSetter i o s t a b = Optic An_IxSetter i o s t a b

-- | Type synonym for a type-preserving indexed setter.
type IxSetter' i o s a = Optic' An_IxSetter i o s a

-- | Explicitly cast an optic to an indexeed setter.
toIxSetter :: Is k An_IxSetter => Optic k i o s t a b -> IxSetter i o s t a b
toIxSetter = sub
{-# INLINE toIxSetter #-}

-- | Apply an indexed setter as a modifier.
iover
  :: (CheckIndices i o, Is k An_IxSetter)
  => Optic k i o s t a b
  -> (i -> a -> b) -> s -> t
iover o f = runIxFunArrow (getOptic (toIxSetter o) (IxFunArrow f)) id
{-# INLINE iover #-}

-- | Apply an indexed setter.
iset
  :: (CheckIndices i o, Is k An_IxSetter)
  => Optic k i o s t a b
  -> (i -> b) -> s -> t
iset o f = iover o (\i _ -> f i)
{-# INLINE iset #-}

-- | Build an indexed setter from a function to modify the element(s).
isets
  :: ((i -> a -> b) -> s -> t)
  -> IxSetter j (i -> j) s t a b
isets f = Optic (\(IxFunArrow k) -> IxFunArrow $ \ij -> f $ \i -> k (ij i))
{-# INLINE isets #-}

-- | Indexed setter via the 'FunctorWithIndex' class.
imapped :: FunctorWithIndex i f => IxSetter j (i -> j) (f a) (f b) a b
imapped = isets imap
{-# INLINE imapped #-}
