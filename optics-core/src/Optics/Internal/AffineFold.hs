{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.AffineFold where

import Optics.Internal.Bicontravariant
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for an affine fold.
data An_AffineFold

-- | Constraints corresponding to an affine fold.
type instance Constraints An_AffineFold p =
  (Bicontravariant p, Cochoice p, Choice p, Strong p)

-- | Type synonym for an affine fold.
type AffineFold s a = Optic' An_AffineFold s a

-- | Explicitly cast an optic to an affine fold.
toAffineFold :: Is k An_AffineFold => Optic' k s a -> AffineFold s a
toAffineFold = sub
{-# INLINE toAffineFold #-}

view01 :: Is k An_AffineFold => Optic' k s a -> s -> Maybe a
view01 o = runForgetM (getOptic (toAffineFold o) (ForgetM Just))
{-# INLINE view01 #-}

afolding :: (s -> Maybe a) -> AffineFold s a
afolding f = Optic (contrabimap (\s -> maybe (Left s) Right (f s)) Left . right')
{-# INLINE afolding #-}
