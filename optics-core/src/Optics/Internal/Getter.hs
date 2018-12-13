module Optics.Internal.Getter where

import Data.Void

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a getter.
type Getter i s a = Optic' A_Getter i i s a

-- | Apply a getter.
view1 :: Is k A_Getter => Optic' k i i s a -> s -> a
view1 o = runForget (getOptic (toGetter o) (Forget id))
{-# INLINE view1 #-}

-- | Explicitly cast an optic to a getter.
toGetter :: Is k A_Getter => Optic' k i i s a -> Getter i s a
toGetter = sub
{-# INLINE toGetter #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter i s a
to f = Optic (dimap f absurd . contrasecond absurd)
{-# INLINE to #-}
