module Optics.Internal.IxSetter where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Internal implementation of 'imapped'.
imapped__
  :: (Mapping p, FunctorWithIndex i f)
  => Optic__ p j (i -> j) (f a) (f b) a b
imapped__ = conjoined' (roam fmap) (iroam imap)
{-# INLINE imapped__ #-}
