module Optics.Internal.IxSetter where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying indexed setter.
type IxSetter i s t a b = Optic A_Setter '[i] s t a b

-- | Type synonym for a type-preserving indexed setter.
type IxSetter' i s a = Optic' A_Setter '[i] s a

-- | Explicitly cast an optic to an indexeed setter.
toIxSetter :: Is k A_Setter => Optic k '[i] s t a b -> IxSetter i s t a b
toIxSetter = sub
{-# INLINE toIxSetter #-}

-- | Apply an indexed setter as a modifier.
iover
  :: (CheckIndices i is, Is k A_Setter)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover o f = runIxFunArrow (getOptic (toIxSetter o) (IxFunArrow f)) id
{-# INLINE iover #-}

-- | Apply an indexed setter.
iset
  :: (CheckIndices i is, Is k A_Setter)
  => Optic k '[i] s t a b
  -> (i -> b) -> s -> t
iset o f = iover o (\i _ -> f i)
{-# INLINE iset #-}

-- | Build an indexed setter from a function to modify the element(s).
isets
  :: ((i -> a -> b) -> s -> t)
  -> IxSetter i s t a b
isets f = Optic (iroam f)
{-# INLINE isets #-}

-- | Indexed setter via the 'FunctorWithIndex' class.
imapped :: FunctorWithIndex i f => IxSetter i (f a) (f b) a b
imapped = isets imap
{-# INLINE imapped #-}
