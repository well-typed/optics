-- | A @'Setter' S T A B@ has the ability to lift a function of type
-- @A -> B@ 'over' a function of type @S -> T@, applying the function
-- to update all the @A@s contained in @S@.  This can be used to 'set'
-- all the @A@s to a single value (by lifting a constant function).
--
-- This can be seen as a generalisation of 'fmap', where the type @S@
-- does not need to be a type constructor with @A@ as its last
-- parameter.
--
module Optics.Setter
  (
  -- * Formation
    Setter
  , Setter'

  -- * Introduction
  , sets
  , mapped

  -- * Elimination
  , set
  , set'
  , over
  , over'

  -- * Subtyping
  , A_Setter
  , toSetter

  -- * Computation
  -- |
  --
  -- @
  -- 'over' ('sets' f) g s = f g s
  -- 'set' ('sets' f) v s = f ('const' v) s
  -- @

  -- * Re-exports
  , module Optics.Optic
  ) where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Setter
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
over o = \f -> runFunArrow $ getOptic (toSetter o) (FunArrow f)
{-# INLINE over #-}

-- | Apply a setter as a modifier, strictly.
--
-- TODO DOC: what exactly is the strictness property?
--
-- Example:
--
-- @
--  f :: Int -> (Int, a) -> (Int, a)
--  f k acc
--    | k > 0     = f (k - 1) $ 'over'' 'Data.Tuple.Optics._1' (+1) acc
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
over' o = \f ->
  let star = getOptic (toSetter o) $ Star (wrapIdentity' . f)
  in unwrapIdentity' . runStar star
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
--
-- TODO DOC: what exactly is the strictness property?
--
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

-- | Create a 'Setter' for a 'Functor'.  Observe that @'over'
-- 'mapped'@ is just 'fmap'.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = Optic mapped__
{-# INLINE mapped #-}
