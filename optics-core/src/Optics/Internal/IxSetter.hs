module Optics.Internal.IxSetter where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Internal implementation of 'imapped'.
imapped__
  :: (Mapping p, FunctorWithIndex i f)
  => Optic__ p j (i -> j) (f a) (f b) a b
imapped__ = conjoinedSets__ fmap imap
{-# INLINE imapped__ #-}

-- | Internal implementation of 'conjoinedSets'.
conjoinedSets__
  :: Mapping p
  => ((     a -> b) -> s -> t)
  -> ((i -> a -> b) -> s -> t)
  -> Optic__ p j (i -> j) s t a b
conjoinedSets__ f g = conjoined (roam f) (iroam g)
{-# INLINE conjoinedSets__ #-}
