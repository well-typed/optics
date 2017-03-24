{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Prism where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a prism.
data A_Prism

-- | Constraints corresponding to a prism.
type instance Constraints A_Prism p = (Choice p)

-- | Type synonym for a type-modifying prism.
type Prism s t a b = Optic A_Prism s t a b

-- | Type synonym for a type-preserving prism.
type Prism' s a = Optic' A_Prism s a

-- | Explicitly cast an optic to a prism.
toPrism :: Is k A_Prism => Optic k s t a b -> Prism s t a b
toPrism = sub
{-# INLINE toPrism #-}

-- | Create a prism.
mkPrism :: Optic_ A_Prism s t a b -> Prism s t a b
mkPrism = Optic
{-# INLINE mkPrism #-}

-- | Build a prism from a constructor and a matcher.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism construct match =
  mkPrism (dimap match (either id construct) . right')
{-# INLINE prism #-}

-- withPrism
-- matching
-- Market

