{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Setter where

import Optics.Internal.Optic

-- | Tag for a setter.
data A_Setter

-- | Constraints corresponding to a setter.
type instance Constraints A_Setter p = (p ~ (->))

-- | Type synonym for a type-modifying setter.
type Setter s t a b = Optic A_Setter s t a b

-- | Type synonym for a type-preserving setter.
type Setter' s a = Optic' A_Setter s a

-- | Explicitly cast an optic to a setter.
toSetter :: Is k A_Setter => Optic k s t a b -> Setter s t a b
toSetter = sub
{-# INLINE toSetter #-}

-- | Create a setter.
mkSetter :: Optic_ A_Setter s t a b -> Setter s t a b
mkSetter = Optic
{-# INLINE mkSetter #-}

-- | Build a setter.
sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets f = Optic f
{-# INLINE sets #-}

-- | Apply a setter as a modifier.
over :: Is k A_Setter => Optic k s t a b -> (a -> b) -> s -> t
over o = getOptic (toSetter o)
{-# INLINE over #-}

-- | Apply a setter.
set :: Is k A_Setter => Optic k s t a b -> b -> s -> t
set o = over o . const
{-# INLINE set #-}
