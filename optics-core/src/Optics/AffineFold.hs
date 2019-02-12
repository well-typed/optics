-- | TODO: what's affine fold.
module Optics.AffineFold
  ( An_AffineFold
  , AffineFold
  , preview
  , afolding
  , afailing
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

-- | Try the first 'AffineFold'. If it returns no entry, try the second one.
--
-- >>> preview (ix 1 % re _Left `afailing` ix 2 % re _Right) [0,1,2,3]
-- Just (Left 1)
--
-- >>> preview (ix 42 % re _Left `afailing` ix 2 % re _Right) [0,1,2,3]
-- Just (Right 2)
--
-- /Note:/ There is no 'summing' equivalent, because @asumming = afailing@.
--
afailing
  :: (Is k An_AffineFold, Is l An_AffineFold)
  => Optic' k is s a
  -> Optic' l js s a
  -> AffineFold s a
afailing a b = afolding $ \s -> maybe (preview b s) Just (preview a s)
infixl 3 `afailing` -- Same as (<|>)
{-# INLINE afailing #-}

-- $setup
-- >>> import Optics.Core
-- >>> import Data.Maybe (listToMaybe)
