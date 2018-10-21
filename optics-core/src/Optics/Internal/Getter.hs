{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Getter where

import Optics.Internal.Bicontravariant
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a getter.
data A_Getter

-- | Constraints corresponding to a getter.
type instance Constraints A_Getter p = (Bicontravariant p, Cochoice p, Strong p)

-- | Type synonym for a getter.
type Getter s a = Optic' A_Getter s a

-- | Apply a getter.
view :: Is k A_Getter => Optic' k s a -> s -> a
view o = runForget (getOptic (toGetter o) (Forget id))
{-# INLINE view #-}

-- | Explicitly cast an optic to a getter.
toGetter :: Is k A_Getter => Optic' k s a -> Getter s a
toGetter = sub
{-# INLINE toGetter #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (contrabimap f f)
{-# INLINE to #-}
