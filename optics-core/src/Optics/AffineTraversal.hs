module Optics.AffineTraversal
  ( An_AffineTraversal
  , AffineTraversal
  , AffineTraversal'
  , toAffineTraversal
  , atraversal
  , withAffineTraversal
  , module Optics.Optic
  )
  where

import Optics.Internal.Concrete
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a type-modifying affine traversal.
type AffineTraversal s t a b = Optic An_AffineTraversal NoIx s t a b

-- | Type synonym for a type-preserving affine traversal.
type AffineTraversal' s a = Optic' An_AffineTraversal NoIx s a

-- | Explicitly cast an optic to an affine traversal.
toAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k                  is s t a b
  -> Optic An_AffineTraversal is s t a b
toAffineTraversal = castOptic
{-# INLINE toAffineTraversal #-}

-- | Build an affine traversal from a matcher and an updater.
atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
atraversal match update = Optic $
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
{-# INLINE atraversal #-}

-- With with an affine traversal as a matcher and an updater.
withAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k is s t a b
  -> ((s -> Either t a) -> (s -> b -> t) -> r)
  -> r
withAffineTraversal o k =
  case getOptic (toAffineTraversal o) (AffineMarket (\_ b -> b) Right) of
    AffineMarket update match -> k match update
