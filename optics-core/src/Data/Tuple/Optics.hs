module Data.Tuple.Optics
  ( _1
  , _2
  , curried
  , uncurried
  , swapped
  )
  where

import Data.Tuple

import Optics.Internal.Profunctor
import Optics.Iso
import Optics.Lens

-- | Lens for the first component of a pair.
--
-- TODO: Introduce a 'Field1' class?
--
_1 :: Lens (a, b) (c, b) a c
_1 = mkLens first'
{-# INLINE _1 #-}

-- | Lens for the second component of a pair.
--
-- TODO: Introduce a 'Field2' class?
--
_2 :: Lens (a, b) (a, c) b c
_2 = mkLens second'
{-# INLINE _2 #-}

-- | Iso between the curried and uncurried versions of a function.
curried :: Iso ((a, b) -> c) ((d, e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry
{-# INLINE curried #-}

-- | Flipped version of 'curried'.
uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a, b) -> c) ((d, e) -> f)
uncurried = iso uncurry curry
{-# INLINE uncurried #-}

-- | Iso that swaps the two components of a pair.
--
-- TODO: Introduce a 'Swapped' class?
--
swapped :: Iso (a, b) (c, d) (b, a) (d, c)
swapped = iso swap swap
{-# INLINE swapped #-}
