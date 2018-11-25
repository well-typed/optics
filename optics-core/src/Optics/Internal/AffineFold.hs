module Optics.Internal.AffineFold where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for an affine fold.
type AffineFold i s a = Optic' An_AffineFold i i s a

-- | Explicitly cast an optic to an affine fold.
toAffineFold :: Is k An_AffineFold => Optic' k i i s a -> AffineFold i s a
toAffineFold = sub
{-# INLINE toAffineFold #-}

view01 :: Is k An_AffineFold => Optic' k i i s a -> s -> Maybe a
view01 o = runForgetM (getOptic (toAffineFold o) (ForgetM Just))
{-# INLINE view01 #-}

afolding :: (s -> Maybe a) -> AffineFold i s a
afolding f = Optic (contrabimap (\s -> maybe (Left s) Right (f s)) Left . right')
{-# INLINE afolding #-}
