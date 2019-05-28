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

  -- * Semigroup structure
  , iafailing

  -- * Subtyping
  , An_AffineFold

  -- * Re-exports
  , module Optics.Optic
  ) where

import Optics.AffineFold
import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Profunctor
import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for an indexed affine fold.
type IxAffineFold i s a = Optic' An_AffineFold (WithIx i) s a

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

-- | Create an 'IxAffineFold' from a partial function.
iafolding :: (s -> Maybe (i, a)) -> IxAffineFold i s a
iafolding g = Optic
  $ ivisit (\point f s -> maybe (point s) (uncurry f) $ g s)
  . rphantom
{-# INLINE iafolding #-}

-- | Try the first 'IxAffineFold'. If it returns no entry, try the second one.
--
-- /Note:/ There is no 'Optics.IxFold.isumming' equivalent, because @iasumming = iafailing@.
iafailing
  :: (Is k An_AffineFold, Is l An_AffineFold,
      is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic' k is1 s a
  -> Optic' l is2 s a
  -> IxAffineFold i s a
iafailing a b = Optic $ conjoined__ (afailing a b) $ iafolding $ \s ->
  maybe (ipreview b s) Just (ipreview a s)
infixl 3 `iafailing` -- Same as (<|>)
{-# INLINE iafailing #-}
