{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Getting where

import Control.Applicative (Const(..))

import Optics.Internal.Optic

-- | Tag for a getting.
data A_Getting r

-- | Constraints corresponding to a getting.
type instance Constraints (A_Getting r) p f =
  (p ~ (->), f ~ Const r)

-- | Type synonym for a getting.
type Getting r s a = Optic' (A_Getting r) s a

-- | Explicitly cast an optic to a getting.
toGetting :: Is k (A_Getting r) => Optic' k s a -> Getting r s a
toGetting = sub
{-# INLINE toGetting #-}

-- | Create a getting.
mkGetting :: Optic_' (A_Getting r) s a -> Getting r s a
mkGetting = Optic
{-# INLINE mkGetting #-}

-- | Apply a getting.
view :: Is k (A_Getting a) => Optic' k s a -> s -> a
view o = getConst . getOptic (toGetting o) Const
{-# INLINE view #-}
