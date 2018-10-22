{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Setter where

import Optics.Internal.Optic

-- | Type synonym for a type-modifying setter.
type Setter s t a b = Optic A_Setter s t a b

-- | Type synonym for a type-preserving setter.
type Setter' s a = Optic' A_Setter s a

-- | Explicitly cast an optic to a setter.
toSetter :: Is k A_Setter => Optic k s t a b -> Setter s t a b
toSetter = sub
{-# INLINE toSetter #-}

-- | Build a setter from a function to modify the element(s).
sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets = Optic
{-# INLINE sets #-}

-- | Build a setter from a functor.
mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

-- | Apply a setter as a modifier.
over :: Is k A_Setter => Optic k s t a b -> (a -> b) -> s -> t
over o = getOptic (toSetter o)
{-# INLINE over #-}

-- | Apply a setter.
set :: Is k A_Setter => Optic k s t a b -> b -> s -> t
set o = over o . const
{-# INLINE set #-}
