{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of indexed setters.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.IxSetter where

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Optic

-- | Internal implementation of 'Optics.IxSetter.imapped'.
imapped__
  :: (Mapping p, FunctorWithIndex i f)
  => Optic__ p j (i -> j) (f a) (f b) a b
imapped__ = conjoined__ (roam fmap) (iroam imap)
{-# INLINE imapped__ #-}
