-- |
-- Module: Optics.Setter
-- Description: Applies an update to all contained values.
--
-- A @'Setter' S T A B@ has the ability to lift a function of type
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

  -- * Elimination
  , over

  -- * Computation
  -- |
  --
  -- @
  -- 'over' ('sets' f) ≡ f
  -- @

  -- * Well-formedness
  -- |
  --
  -- * __PutPut__: Setting twice is the same as setting once:
  --
  --     @
  --     'Optics.Setter.set' l v' ('Optics.Setter.set' l v s) ≡ 'Optics.Setter.set' l v' s
  --     @
  --
  -- * __Functoriality__: 'Setter's must preserve identities and composition:
  --
  --     @
  --     'over' s 'id' ≡ 'id'
  --     'over' s f '.' 'over' s g ≡ 'over' s (f '.' g)
  --     @

  -- * Additional introduction forms
  -- | See also 'Data.Set.Optics.setmapped', which changes the elements of a 'Data.Set.Set'.
  , mapped

  -- * Additional elimination forms
  , set
  , set'
  , over'

  -- * Subtyping
  , A_Setter
  -- | <<diagrams/Setter.png Setter in the optics hierarchy>>

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

-- | Apply a setter as a modifier.
over
  :: Is k A_Setter
  => Optic k is s t a b
  -> (a -> b) -> s -> t
over o = \f -> runFunArrow $ getOptic (castOptic @A_Setter o) (FunArrow f)
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
  let star = getOptic (castOptic @A_Setter o) $ Star (wrapIdentity' . f)
  in unwrapIdentity' . runStar star
{-# INLINE over' #-}

-- | Apply a setter.
--
-- @
-- 'set' o v ≡ 'over' o ('const' v)
-- @
--
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

-- | Build a setter from a function to modify the element(s), which must respect
-- the well-formedness laws.
sets
  :: ((a -> b) -> s -> t)
  -> Setter s t a b
sets f = Optic (roam f)
{-# INLINE sets #-}

-- | Create a 'Setter' for a 'Functor'.
--
-- @
-- 'over' 'mapped' ≡ 'fmap'
-- @
--
mapped :: Functor f => Setter (f a) (f b) a b
mapped = Optic mapped__
{-# INLINE mapped #-}

-- $setup
-- >>> import Optics.Core
