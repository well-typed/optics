module Optics.Internal.Setter where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

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

-- | Build a setter from a function to modify the element(s).
sets
  :: ((a -> b) -> s -> t)
  -> Setter s t a b
sets f = Optic (roam f)
{-# INLINE sets #-}

-- | Setter via the 'Functor' class.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap
{-# INLINE mapped #-}

-- $setup
-- >>> import Optics.Core
