{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Getter where

import Control.Applicative (Const(..))

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a getter.
data A_Getter

-- | Constraints corresponding to a getter.
type instance Constraints A_Getter p f = (p ~ (->), Contravariant f, Functor f)

-- | Type synonym for a getter.
type Getter s a = Optic' A_Getter s a

-- | Explicitly cast an optic to a getter.
toGetter :: Is k A_Getter => Optic' k s a -> Getter s a
toGetter = sub
{-# INLINE toGetter #-}

-- | Create a getter.
mkGetter :: Optic_' A_Getter s a -> Getter s a
mkGetter = Optic
{-# INLINE mkGetter #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (dimap f (contramap f))
{-# INLINE to #-}
