module Optics.Internal.Setter where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a type-modifying setter.
type Setter i s t a b = Optic A_Setter i i s t a b

-- | Type synonym for a type-preserving setter.
type Setter' i s a = Optic' A_Setter i i s a

-- | Explicitly cast an optic to a setter.
toSetter
  :: Is k A_Setter
  => Optic k i o s t a b
  -> Optic A_Setter i o s t a b
toSetter = sub
{-# INLINE toSetter #-}

-- | Apply a setter as a modifier.
over
  :: Is k A_Setter
  => Optic k i o s t a b
  -> (a -> b) -> s -> t
over o = runFunArrow #. getOptic (toSetter o) .# FunArrow
{-# INLINE over #-}

-- | Apply a setter.
--
-- >>> set _1 'x' ('y', 'z')
-- ('x','z')
--
set
  :: Is k A_Setter
  => Optic k i o s t a b
  -> b -> s -> t
set o = over o . const
{-# INLINE set #-}

-- | Build a setter from a function to modify the element(s).
sets
  :: ((a -> b) -> s -> t)
  -> Setter i s t a b
sets f = Optic (dimap (Context id) (\(Context g s) -> f g s) . map')
{-# INLINE sets #-}

-- | Setter via the 'Functor' class.
mapped :: Functor f => Setter i (f a) (f b) a b
mapped = Optic map'
{-# INLINE mapped #-}

-- $setup
-- >>> import Optics
