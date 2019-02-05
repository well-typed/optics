module Optics.Setter
  ( A_Setter
  , Setter
  , Setter'
  , toSetter
  , over
  , over'
  , set
  , set'
  , sets
  , mapped
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Setter
import Optics.Internal.Utils
import Optics.Optic

-- | Type synonym for a type-modifying setter.
type Setter s t a b = Optic A_Setter NoIx s t a b

-- | Type synonym for a type-preserving setter.
type Setter' s a = Optic' A_Setter NoIx s a

-- | Explicitly cast an optic to a setter.
toSetter
  :: Is k A_Setter
  => Optic k is s t a b
  -> Optic A_Setter is s t a b
toSetter = castOptic
{-# INLINE toSetter #-}

-- | Apply a setter as a modifier.
over
  :: Is k A_Setter
  => Optic k is s t a b
  -> (a -> b) -> s -> t
over o = runFunArrow #. getOptic (toSetter o) .# FunArrow
{-# INLINE over #-}

-- | Apply a setter as a modifier, strictly.
--
-- Example:
--
-- @
--  f :: Int -> (Int, a) -> (Int, a)
--  f k acc
--    | k > 0     = f (k - 1) $ over' _1 (+1) acc
--    | otherwise = acc
-- @
--
-- runs in constant space, but would result in a space leak if used with 'over'.
--
-- Note that replacing '$' with '$!' or 'Data.Tuple.Optics._1' with
-- 'Data.Tuple.Optics._1'' (which amount to the same thing) doesn't help when
-- 'over' is used, because the first coordinate of a pair is never forced.
--
over'
  :: Is k A_Setter
  => Optic k is s t a b
  -> (a -> b) -> s -> t
over' o f = unwrapIdentity' . runStar star
  where
    star = getOptic (toSetter o) $ Star (wrapIdentity' . f)
{-# INLINE over' #-}

-- | Apply a setter.
--
-- >>> let _1  = lens fst $ \(_,y) x -> (x, y)
-- >>> set _1 'x' ('y', 'z')
-- ('x','z')
--
set
  :: Is k A_Setter
  => Optic k is s t a b
  -> b -> s -> t
set o = over o . const
{-# INLINE set #-}

-- | Apply a setter, strictly.
set'
  :: Is k A_Setter
  => Optic k is s t a b
  -> b -> s -> t
set' o = over' o . const
{-# INLINE set' #-}

-- | Build a setter from a function to modify the element(s).
sets
  :: ((a -> b) -> s -> t)
  -> Setter s t a b
sets f = Optic (roam f)
{-# INLINE sets #-}

-- | Setter via the 'Functor' class.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = Optic mapped__
{-# INLINE mapped #-}
