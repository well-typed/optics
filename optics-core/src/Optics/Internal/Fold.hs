{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Fold where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a fold.
data A_Fold

-- | Constraints corresponding to a fold.
type instance Constraints A_Fold p f =
  (p ~ (->), Contravariant f, Applicative f)

-- | Type synonym for a fold.
type Fold s a = Optic' A_Fold s a

-- | Explicitly cast an optic to a fold.
toFold :: Is k A_Fold => Optic' k s a -> Fold s a
toFold = sub
{-# INLINE toFold #-}

-- | Create a fold.
mkFold :: Optic_' A_Fold s a -> Fold s a
mkFold = Optic
{-# INLINE mkFold #-}
