{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Optics.Generic.Internal.Utils where

import Data.Functor.Identity
import qualified Data.Generics.Internal.VL.Prism as GL
import qualified Data.Profunctor as P

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils
import Optics.Prism

newtype  WrappedProfunctor p i ci a b =
  WrapProfunctor { unwrapProfunctor :: p i ci a b }

instance Profunctor p => P.Profunctor (WrappedProfunctor p i ci) where
  dimap f g (WrapProfunctor pab) = WrapProfunctor (dimap f g pab)
  lmap  f   (WrapProfunctor pab) = WrapProfunctor (lmap  f   pab)
  rmap    g (WrapProfunctor pab) = WrapProfunctor (rmap    g pab)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice p => P.Choice (WrappedProfunctor p i ci) where
  left'  (WrapProfunctor pab) = WrapProfunctor (left'  pab)
  right' (WrapProfunctor pab) = WrapProfunctor (right' pab)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

-- | Build a 'Prism' from the van Laarhoven representation.
prismVL :: forall s t a b. GL.Prism s t a b -> Prism s t a b
prismVL f = Optic $ rcoerce @(Identity t) @t
                  . (unwrapProfunctor #. f .# WrapProfunctor)
                  . rcoerce @b @(Identity b)
{-# INLINE prismVL #-}
