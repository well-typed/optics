module Optics.AffineTraversal
  ( An_AffineTraversal
  , AffineTraversal
  , AffineTraversal'
  , toAffineTraversal
  , atraversal
  , atraversal'
  , withAffineTraversal
  , AffineTraversalVL
  , AffineTraversalVL'
  , atraversalVL
  , toAtraversalVL
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

-- | Type synonym for a type-modifying van Laarhoven affine traversal.
type AffineTraversalVL s t a b =
  forall f. Functor f => (forall r. r -> f r) -> (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven affine traversal.
type AffineTraversalVL' s a = AffineTraversalVL s s a a

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
  visit $ \point f s -> either point (\a -> update s <$> f a) (match s)
{-# INLINE atraversal #-}

-- | Build a type-preserving affine traversal from a matcher and an updater.
atraversal' :: (s -> Maybe a) -> (s -> b -> s) -> AffineTraversal s s a b
atraversal' sma sbs = atraversal (\s -> maybe (Left s) Right (sma s)) sbs
{-# INLINE atraversal' #-}

-- With with an affine traversal as a matcher and an updater.
withAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k is s t a b
  -> ((s -> Either t a) -> (s -> b -> t) -> r)
  -> r
withAffineTraversal o k =
  case getOptic (toAffineTraversal o) (AffineMarket (\_ b -> b) Right) of
    AffineMarket update match -> k match update
{-# INLINE withAffineTraversal #-}

-- | Build an affine traversal from the van Laarhoven representation.
atraversalVL :: AffineTraversalVL s t a b -> AffineTraversal s t a b
atraversalVL f = Optic (visit f)
{-# INLINE atraversalVL #-}

-- | Convert an affine traversal to its van Laarhoven representation.
toAtraversalVL
  :: Is k An_AffineTraversal
  => Optic k is s t a b
  -> AffineTraversalVL s t a b
toAtraversalVL o point =
  runStarA . getOptic (toAffineTraversal o) . StarA point
{-# INLINE toAtraversalVL #-}
