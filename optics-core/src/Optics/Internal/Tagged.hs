{-# OPTIONS_HADDOCK not-home #-}

-- | Based on the @tagged@ package.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
--
module Optics.Internal.Tagged where

import Data.Coerce

import Optics.Internal.Bi
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Tag a value with not one but two phantom type parameters (so that 'Tagged'
-- can be used as an indexed profunctor).
newtype Tagged i a b = Tagged { unTagged :: b }

instance Functor (Tagged i a) where
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
