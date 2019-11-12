-- |
-- Module: Optics.IxAffineTraversal
-- Description: An indexed version of an 'Optics.AffineTraversal.AffineTraversal'.
--
-- An 'IxAffineTraversal' is an indexed version of an
-- 'Optics.AffineTraversal.AffineTraversal'.  See the "Indexed optics" section
-- of the overview documentation in the @Optics@ module of the main @optics@
-- package for more details on indexed optics.
--
module Optics.IxAffineTraversal
  (
  -- * Formation
    IxAffineTraversal
  , IxAffineTraversal'

  -- * Introduction
  , iatraversal

  -- * Elimination
  -- | An 'IxAffineTraversal' is in particular an
  -- 'Optics.IxAffineFold.IxAffineFold' and an 'Optics.IxSetter.IxSetter',
  -- therefore you can specialise types to obtain:
  --
  -- @
  -- 'Optics.IxAffineFold.ipreview' :: 'IxAffineTraversal' i s t a b -> s -> Maybe (i, a)
  -- @
  --
  -- @
  -- 'Optics.IxSetter.iover'    :: 'IxAffineTraversal' i s t a b -> (i -> a -> b) -> s -> t
  -- 'Optics.IxSetter.iset'     :: 'IxAffineTraversal' i s t a b -> (i      -> b) -> s -> t
  -- @

  -- * Subtyping
  , An_AffineTraversal

  -- * van Laarhoven encoding
  , IxAffineTraversalVL
  , IxAffineTraversalVL'
  , iatraversalVL
  , toIxAtraversalVL
  ) where

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Type synonym for a type-modifying indexed affine traversal.
type IxAffineTraversal i s t a b = Optic An_AffineTraversal (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed affine traversal.
type IxAffineTraversal' i s a = Optic' An_AffineTraversal (WithIx i) s a

-- | Type synonym for a type-modifying van Laarhoven indexed affine traversal.
--
-- Note: this isn't exactly van Laarhoven representation as there is no
-- @Pointed@ class (which would be a superclass of 'Applicative' that contains
-- 'pure' but not '<*>'). You can interpret the first argument as a dictionary
-- of @Pointed@ that supplies the @point@ function (i.e. the implementation of
-- 'pure').
--
type IxAffineTraversalVL i s t a b =
  forall f. Functor f => (forall r. r -> f r) -> (i -> a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven indexed affine traversal.
type IxAffineTraversalVL' i s a = IxAffineTraversalVL i s s a a

-- | Build an indexed affine traversal from a matcher and an updater.
--
-- If you want to build an 'IxAffineTraversal' from the van Laarhoven
-- representation, use 'iatraversalVL'.
iatraversal :: (s -> Either t (i, a)) -> (s -> b -> t) -> IxAffineTraversal i s t a b
iatraversal match update = iatraversalVL $ \point f s ->
  either point (\a -> update s <$> uncurry' f a) (match s)
{-# INLINE iatraversal #-}

-- | Build an indexed affine traversal from the van Laarhoven representation.
iatraversalVL :: IxAffineTraversalVL i s t a b -> IxAffineTraversal i s t a b
iatraversalVL f = Optic (ivisit f)
{-# INLINE iatraversalVL #-}

-- | Convert an indexed affine traversal to its van Laarhoven representation.
toIxAtraversalVL
  :: (Is k An_AffineTraversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxAffineTraversalVL i s t a b
toIxAtraversalVL o point = \f ->
  runIxStarA (getOptic (castOptic @An_AffineTraversal o) (IxStarA point f)) id
{-# INLINE toIxAtraversalVL #-}
