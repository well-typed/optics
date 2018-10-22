{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.AffineTraversal where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying affine traversal.
type AffineTraversal s t a b = Optic An_AffineTraversal s t a b

-- | Type synonym for a type-preserving affine traversal.
type AffineTraversal' s a = Optic' An_AffineTraversal s a

-- | Explicitly cast an optic to an affine traversal.
toAffineTraversal
  :: Is k An_AffineTraversal => Optic k s t a b -> AffineTraversal s t a b
toAffineTraversal = sub
{-# INLINE toAffineTraversal #-}

-- | Build an affine traversal from a matcher and an updater.
atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
atraversal match update = Optic $
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
