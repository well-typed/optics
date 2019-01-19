module Optics.Internal.AffineFold where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

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
-- >>> view01 _Right (Right 'x')
-- Just 'x'
--
-- >>> view01 _Right (Left 'y')
-- Nothing
--
view01 :: Is k An_AffineFold => Optic' k is s a -> s -> Maybe a
view01 o = runForgetM (getOptic (toAffineFold o) (ForgetM Just))
{-# INLINE view01 #-}

-- | Create an 'AffineFold' from a partial function.
--
-- >>> view01 (afold listToMaybe) "foo"
-- Just 'f'
--
afold :: (s -> Maybe a) -> AffineFold s a
afold f = Optic (contrabimap (\s -> maybe (Left s) Right (f s)) Left . right')
{-# INLINE afold #-}

-- $setup
-- >>> import Optics.Core
-- >>> import Data.Maybe (listToMaybe)
