-- |
-- Module: Optics.AffineTraversal
-- Description: A 'Optics.Traversal.Traversal' that applies to at most one element.
--
-- An 'AffineTraversal' is a 'Optics.Traversal.Traversal' that
-- applies to at most one element.
--
-- These arise most frequently as the composition of a
-- 'Optics.Lens.Lens' with a 'Optics.Prism.Prism'.
--
module Optics.AffineTraversal
  (
  -- * Formation
    AffineTraversal
  , AffineTraversal'

  -- * Introduction
  , atraversal

  -- * Elimination
  -- | An 'AffineTraversal' is a 'Optics.Setter.Setter', therefore you can
  -- specialise types to obtain:
  --
  -- @
  -- 'Optics.Setter.set' :: 'AffineTraversal' s t a b -> b -> s -> t
  -- @
  , matching

  -- * Computation
  -- |
  --
  -- @
  -- 'matching' ('atraversal' f g) ≡ f
  -- 'Optics.Setter.set'      ('atraversal' f g) ≡ 'flip' g
  -- @

  -- * Additional elimination forms
  , withAffineTraversal

  -- * Subtyping
  , An_AffineTraversal
  , toAffineTraversal

  -- * van Laarhoven encoding
  , AffineTraversalVL
  , AffineTraversalVL'
  , atraversalVL
  , toAtraversalVL

  -- * Re-exports
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
--
-- Note: this isn't exactly van Laarhoven representation as there is
-- no @Pointed@ class (which would be a superclass of 'Applicative'
-- that contains 'pure' but not '<*>'). You can interpret the first
-- argument as a dictionary of @Pointed@ that supplies the @point@
-- function (i.e. the implementation of 'pure').
--
-- A 'Optics.Traversal.TraversalVL' has 'Applicative' available and
-- hence can combine the effects arising from multiple elements using
-- '<*>'. In contrast, an 'AffineTraversalVL' has no way to combine
-- effects from multiple elements, so it must act on at most one
-- element.  (It can act on none at all thanks to the availability of
-- @point@.)
--
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
  -- Do not define atraversal in terms of atraversalVL, mixing profunctor-style
  -- definitions with VL style implementation can lead to subpar generated code.
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
{-# INLINE atraversal #-}

-- | Work with an affine traversal as a matcher and an updater.
withAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k is s t a b
  -> ((s -> Either t a) -> (s -> b -> t) -> r)
  -> r
withAffineTraversal o = \k ->
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
toAtraversalVL o point = runStarA . getOptic (toAffineTraversal o) . StarA point
{-# INLINE toAtraversalVL #-}

-- | Retrieve the value targeted by an 'AffineTraversal' or return the original
-- value while allowing the type to change if it does not match.
--
-- @
-- 'Optics.AffineFold.preview' o ≡ 'either' ('const' 'Nothing') 'id' . 'matching' o
-- @
matching :: Is k An_AffineTraversal => Optic k is s t a b -> s -> Either t a
matching o = withAffineTraversal o $ \match _ -> match
{-# INLINE matching #-}
