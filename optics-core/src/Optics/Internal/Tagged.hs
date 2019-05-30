{-# OPTIONS_HADDOCK not-home #-}

-- | Taken from the tagged package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
--
module Optics.Internal.Tagged where

import Data.Coerce
import Data.Functor.Const

import Optics.Internal.Bi
import Optics.Internal.Profunctor
import Optics.Internal.Utils

newtype Tagged l i a b = Tagged { unTagged :: b }

instance Functor (Tagged l i a) where
  fmap f = Tagged #. f .# unTagged
  {-# INLINE fmap #-}

instance Bifunctor Tagged where
  bimap  _f g = Tagged #. g .# unTagged
  first  _f   = coerce
  second    g = Tagged #. g .# unTagged
  {-# INLINE bimap #-}
  {-# INLINE first #-}
  {-# INLINE second #-}

instance Profunctor Tagged where
  dimap _f g = Tagged #. g .# unTagged
  lmap  _f   = coerce
  rmap     g = Tagged #. g .# unTagged
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice Tagged where
  left'  = Tagged #. Left  .# unTagged
  right' = Tagged #. Right .# unTagged
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Costrong Tagged where
  unfirst (Tagged bd) = Tagged (fst bd)
  unsecond (Tagged db) = Tagged (snd db)
  {-# INLINE unfirst #-}
  {-# INLINE unsecond #-}

  colinear  f (Tagged k) = Tagged $ getConst (f Const k)
  icolinear f (Tagged k) = Tagged $ getConst (f (\_ -> Const) k)
  {-# INLINE colinear #-}
  {-# INLINE icolinear #-}
