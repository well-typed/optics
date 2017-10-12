{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Setter where

import Data.Coerce (coerce)
import Data.Functor.Identity

import Optics.Internal.Optic

-- | Tag for a setter.
data A_Setter

-- | Constraints corresponding to a setter.
type instance Constraints A_Setter p f = (p ~ (->), f ~ Identity)

-- | Type synonym for a type-modifying setter.
type Setter s t a b = Optic A_Setter s t a b

-- | Type synonym for a type-preserving setter.
type Setter' s a = Optic' A_Setter s a

-- | Explicitly cast an optic to a setter.
toSetter :: Is k A_Setter => Optic k s t a b -> Setter s t a b
toSetter = sub
{-# INLINE toSetter #-}

-- | Build a setter from the van Laarhoven representation.
vlSetter :: ((a -> Identity b) -> s -> Identity t) -> Setter s t a b
vlSetter x = Optic x
{-# INLINE vlSetter #-}

-- | Build a setter from a function to modify the element(s).
sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets f = Optic (coerce f)
{-# INLINE sets #-}

-- | Build a setter from a functor.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

-- | Apply a setter as a modifier.
over :: Is k A_Setter => Optic k s t a b -> (a -> b) -> s -> t
over o = coerce (getOptic (toSetter o))
{-# INLINE over #-}

-- | Apply a setter.
set :: Is k A_Setter => Optic k s t a b -> b -> s -> t
set o = over o . const
{-# INLINE set #-}
