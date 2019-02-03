module Optics.Getter
  ( A_Getter
  , Getter
  , toGetter
  , view
  , to
  , module Optics.Optic
  )
  where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a getter.
type Getter s a = Optic' A_Getter NoIx s a

-- | Explicitly cast an optic to a getter.
toGetter :: Is k A_Getter => Optic' k is s a -> Optic' A_Getter is s a
toGetter = castOptic
{-# INLINE toGetter #-}

-- | Apply a getter.
view :: Is k A_Getter => Optic' k is s a -> s -> a
view o = runForget (getOptic (toGetter o) (Forget id))
{-# INLINE view #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (lmap f . rphantom)
{-# INLINE to #-}
