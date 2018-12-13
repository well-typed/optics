module Optics.Internal.AffineTraversal where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying affine traversal.
type AffineTraversal i s t a b = Optic An_AffineTraversal i i s t a b

-- | Type synonym for a type-preserving affine traversal.
type AffineTraversal' i s a = Optic' An_AffineTraversal i i s a

-- | Explicitly cast an optic to an affine traversal.
toAffineTraversal
  :: Is k An_AffineTraversal => Optic k i i s t a b -> AffineTraversal i s t a b
toAffineTraversal = sub
{-# INLINE toAffineTraversal #-}

-- | Build an affine traversal from a matcher and an updater.
atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal i s t a b
atraversal match update = Optic $
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
{-# INLINE atraversal #-}
