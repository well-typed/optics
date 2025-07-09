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

  -- * Combinators
  , unsafeFilteredBy

  -- * Additional introduction forms
  , ignored

  , iadisjoin

  -- * Subtyping
  , An_AffineTraversal

  -- * van Laarhoven encoding
  , IxAffineTraversalVL
  , IxAffineTraversalVL'
  , iatraversalVL
  , iatraverseOf
  ) where

import Data.Profunctor.Indexed

import Optics.AffineFold
import Optics.AffineTraversal
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

-- | Traverse over the target of an 'IxAffineTraversal' and compute a
-- 'Functor'-based answer.
--
-- @since 0.3
iatraverseOf
  :: (Is k An_AffineTraversal, Functor f, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (forall r. r -> f r) -> (i -> a -> f b) -> s -> f t
iatraverseOf o point = \f ->
  runIxStarA (getOptic (castOptic @An_AffineTraversal o) (IxStarA point f)) id
{-# INLINE iatraverseOf #-}

-- | Obtain a potentially empty 'IxAffineTraversal' by taking the element from
-- another 'AffineFold' and using it as an index.
--
-- -- /Note:/ This is /not/ a legal 'Optics.IxTraversal.IxTraversal', unless you
-- are very careful not to invalidate the predicate on the target (see
-- 'Optics.AffineTraversal.unsafeFiltered' for more details).
--
-- @since 0.3
unsafeFilteredBy
  :: Is k An_AffineFold
  => Optic' k is a i
  -> IxAffineTraversal' i a a
unsafeFilteredBy p = iatraversalVL $ \point f s -> case preview p s of
  Just i  -> f i s
  Nothing -> point s
{-# INLINE unsafeFilteredBy #-}

-- | This is the trivial empty 'IxAffineTraversal', i.e. the optic that targets
-- no substructures.
--
-- This is the identity element when a 'Optics.Fold.Fold',
-- 'Optics.AffineFold.AffineFold', 'Optics.IxFold.IxFold',
-- 'Optics.IxAffineFold.IxAffineFold', 'Optics.Traversal.Traversal' or
-- 'Optics.IxTraversal.IxTraversal' is viewed as a monoid.
--
-- >>> 6 & ignored %~ absurd
-- 6
ignored :: IxAffineTraversal i s s a b
ignored = iatraversalVL $ \point _ -> point
{-# INLINE ignored #-}

iadisjoin
  :: ( Is k An_AffineTraversal, Is l An_AffineTraversal
     , is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic k is1 s t a b
  -> Optic l is2 s t a b
  -> IxAffineTraversal i s t a b
iadisjoin a b = conjoined (adisjoin a b) $ iatraversalVL $ \point f s ->
  let OrT visited fu = iatraverseOf a (OrT False . point) (\i -> wrapOrT . f i) s
  in if visited
     then fu
     else iatraverseOf b point f s
infixl 3 `iadisjoin` -- Same as (<|>)
{-# INLINE iadisjoin #-}

-- $setup
-- >>> import Optics.Core
-- >>> import Data.Void (absurd)
