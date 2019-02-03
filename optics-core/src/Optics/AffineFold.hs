-- | TODO: what's affine fold.
module Optics.AffineFold
  ( An_AffineFold
  , AffineFold
  , preview
  , afolding
  , module Optics.Optic
  ) where

import Optics.Internal.Bi
import Optics.Internal.Profunctor
import Optics.Internal.Optic
import Optics.Optic

-- | Type synonym for an affine fold.
type AffineFold s a = Optic' An_AffineFold NoIx s a

-- | Explicitly cast an optic to an affine fold.
toAffineFold
  :: Is k An_AffineFold
  => Optic' k             is s a
  -> Optic' An_AffineFold is s a
toAffineFold = castOptic
{-# INLINE toAffineFold #-}

-- | View through 'AffineFold'.
--
-- >>> let _Right = prism Right $ either (Left . Left) Right
--
-- >>> preview _Right (Right 'x')
-- Just 'x'
--
-- >>> preview _Right (Left 'y')
-- Nothing
--
preview :: Is k An_AffineFold => Optic' k is s a -> s -> Maybe a
preview o = runForgetM (getOptic (toAffineFold o) (ForgetM Just))
{-# INLINE preview #-}

-- | Create an 'AffineFold' from a partial function.
--
-- >>> preview (afolding listToMaybe) "foo"
-- Just 'f'
--
afolding :: (s -> Maybe a) -> AffineFold s a
afolding f = Optic (contrabimap (\s -> maybe (Left s) Right (f s)) Left . right')
{-# INLINE afolding #-}

-- $setup
-- >>> import Optics.Core
-- >>> import Data.Maybe (listToMaybe)
