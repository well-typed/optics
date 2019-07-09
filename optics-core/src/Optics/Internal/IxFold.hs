{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of indexed folds.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.IxFold where

import Data.Functor
import Data.Foldable

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Profunctor
import Optics.Internal.Optic
import Optics.Internal.Fold

-- | Internal implementation of 'Optics.IxFold.ifoldVL'.
ifoldVL__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (i -> a -> f u) -> s -> f v)
  -> Optic__ p j (i -> j) s t a b
ifoldVL__ f = rphantom . iwander f . rphantom
{-# INLINE ifoldVL__ #-}

-- | Internal implementation of 'Optics.IxFold.ifolded'.
ifolded__
  :: (Bicontravariant p, Traversing p, FoldableWithIndex i f)
  => Optic__ p j (i -> j) (f a) t a b
ifolded__ = conjoined__ (foldVL__ traverse_) (ifoldVL__ itraverse_)
{-# INLINE ifolded__ #-}

-- | Internal implementation of 'Optics.IxFold.ifoldring'.
ifoldring__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (i -> a -> f u -> f u) -> f v -> s -> f w)
  -> Optic__ p j (i -> j) s t a b
ifoldring__ fr = ifoldVL__ $ \f -> void . fr (\i a -> (f i a *>)) (pure v)
  where
    v = error "ifoldring__: value used"
{-# INLINE ifoldring__ #-}
