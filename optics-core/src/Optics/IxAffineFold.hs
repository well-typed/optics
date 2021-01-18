-- |
-- Module: Optics.IxAffineFold
-- Description: An indexed version of an 'Optics.AffineFold.AffineFold'.
--
-- An 'IxAffineFold' is an indexed version of an 'Optics.AffineFold.AffineFold'.
-- See the "Indexed optics" section of the overview documentation in the
-- @Optics@ module of the main @optics@ package for more details on indexed
-- optics.
--
module Optics.IxAffineFold
  (
  -- * Formation
    IxAffineFold

  -- * Introduction
  , iafolding

  -- * Elimination
  , ipreview
  , ipreviews

  -- * Computation
  -- |
  --
  -- @
  -- 'ipreview' ('iafolding' f) ≡ f
  -- @

  -- * Additional introduction forms
  , iafoldVL

  -- * Additional elimination forms
  , iatraverseOf_

  -- * Combinators
  , filteredBy

  -- * Monoid structure
  -- | 'IxAffineFold' admits a monoid structure where 'iafailing' combines folds
  -- (returning a result from the second fold only if the first returns none)
  -- and the identity element is 'Optics.IxAffineTraversal.ignored' (which
  -- returns no results).
  --
  -- /Note:/ There is no 'Optics.IxFold.isumming' equivalent that returns an
  -- 'IxAffineFold', because it would not need to return more than one result.
  --
  -- There is no 'Semigroup' or 'Monoid' instance for 'IxAffineFold', because
  -- there is not a unique choice of monoid to use that works for all optics,
  -- and the ('<>') operator could not be used to combine optics of different
  -- kinds.
  , iafailing_

  -- * Subtyping
  , An_AffineFold
  ) where

import Data.Profunctor.Indexed

import Optics.AffineFold
import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Type synonym for an indexed affine fold.
type IxAffineFold i s a = Optic' An_AffineFold (WithIx i) s a

-- | Obtain an 'IxAffineFold' by lifting 'itraverse_' like function.
--
-- @
-- 'aifoldVL' '.' 'iatraverseOf_' ≡ 'id'
-- 'aitraverseOf_' '.' 'iafoldVL' ≡ 'id'
-- @
--
-- @since 0.3
iafoldVL
  :: (forall f. Functor f => (forall r. r -> f r) -> (i -> a -> f u) -> s -> f v)
  -> IxAffineFold i s a
iafoldVL f = Optic (rphantom . ivisit f . rphantom)
{-# INLINE iafoldVL #-}

-- | Retrieve the value along with its index targeted by an 'IxAffineFold'.
ipreview
  :: (Is k An_AffineFold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> s -> Maybe (i, a)
ipreview o = ipreviews o (,)
{-# INLINE ipreview #-}

-- | Retrieve a function of the value and its index targeted by an
-- 'IxAffineFold'.
ipreviews
  :: (Is k An_AffineFold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> r) -> s -> Maybe r
ipreviews o = \f -> runIxForgetM
  (getOptic (castOptic @An_AffineFold o) . IxForgetM $ \i -> Just . f i)
  id
{-# INLINE ipreviews #-}

-- | Traverse over the target of an 'IxAffineFold', computing a 'Functor'-based
-- answer, but unlike 'Optics.IxAffineTraversal.iatraverseOf' do not construct a
-- new structure.
--
-- @since 0.3
iatraverseOf_
  :: (Is k An_AffineFold, Functor f, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (forall r. r -> f r) -> (i -> a -> f u) -> s -> f ()
iatraverseOf_ o point f s = case ipreview o s of
  Just (i, a) -> () <$ f i a
  Nothing     -> point ()
{-# INLINE iatraverseOf_ #-}

-- | Create an 'IxAffineFold' from a partial function.
iafolding :: (s -> Maybe (i, a)) -> IxAffineFold i s a
iafolding g = iafoldVL (\point f s -> maybe (point s) (uncurry' f) $ g s)
{-# INLINE iafolding #-}

-- | Obtain a potentially empty 'IxAffineFold' by taking the element from
-- another 'AffineFold' and using it as an index.
--
-- @since 0.3
filteredBy :: Is k An_AffineFold  => Optic' k is a i -> IxAffineFold i a a
filteredBy p = iafoldVL $ \point f s -> case preview p s of
  Just i  -> f i s
  Nothing -> point s
{-# INLINE filteredBy #-}

-- | Try the first 'IxAffineFold'. If it returns no entry, try the second one.
--
iafailing_
  :: (Is k An_AffineFold, Is l An_AffineFold,
      is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic' k is1 s a
  -> Optic' l is2 s a
  -> IxAffineFold i s a
iafailing_ a b = conjoined (afailing_ a b) $ iafolding $ \s ->
  maybe (ipreview b s) Just (ipreview a s)
infixl 3 `iafailing_` -- Same as (<|>)
{-# INLINE iafailing_ #-}
