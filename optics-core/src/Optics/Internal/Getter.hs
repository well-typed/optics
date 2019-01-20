module Optics.Internal.Getter where

import Data.Void

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a getter.
type Getter s a = Optic' A_Getter NoIx s a

-- | Explicitly cast an optic to a getter.
toGetter :: Is k A_Getter => Optic' k is s a -> Optic' A_Getter is s a
toGetter = castOptic
{-# INLINE toGetter #-}

-- | Apply a getter.
view1 :: Is k A_Getter => Optic' k is s a -> s -> a
view1 o = runForget (getOptic (toGetter o) (Forget id))
{-# INLINE view1 #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (dimap f absurd . contrasecond absurd)
{-# INLINE to #-}
