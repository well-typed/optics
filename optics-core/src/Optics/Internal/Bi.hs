{-# OPTIONS_HADDOCK not-home #-}

-- | Classes for co- and contravariant bifunctors.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Bi where

import Data.Void

import Data.Profunctor.Indexed

----------------------------------------

-- | If @p@ is a 'Profunctor' and a 'Bifunctor' then its left parameter must be
-- phantom.
lphantom :: (Profunctor p, Bifunctor p) => p i a c -> p i b c
lphantom = first_ absurd . lmap absurd

-- | If @p@ is a 'Profunctor' and 'Bicontravariant' then its right parameter
-- must be phantom.
rphantom :: (Profunctor p, Bicontravariant p) => p i c a -> p i c b
rphantom = rmap absurd . contrasecond absurd
