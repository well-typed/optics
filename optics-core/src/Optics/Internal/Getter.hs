{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Getter where

import Data.Void

import Optics.Internal.Bicontravariant
import Optics.Internal.Optic
import Optics.Internal.Profunctor

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
to f = Optic (dimap f absurd . contrasecond absurd)
{-# INLINE to #-}
