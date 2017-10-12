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

-- | Build a getter from the van Laarhoven representation.
mkGetter :: (forall f . (Contravariant f, Functor f) => (a -> f a) -> s -> f s) -> Getter s a
mkGetter = Optic
{-# INLINE mkGetter #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (dimap f (contramap f))
{-# INLINE to #-}

-- | Apply a getter.
view :: Is k A_Getter => Optic' k s a -> s -> a
view o = getConst . getOptic (toGetter o) Const
{-# INLINE view #-}
