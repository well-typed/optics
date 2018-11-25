-- | Taken from the tagged package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Tagged where

import Data.Bifunctor

import Optics.Internal.Profunctor
import Optics.Internal.Utils

newtype Tagged a b = Tagged { unTagged :: b }

instance Functor (Tagged a) where
  fmap f = Tagged #. f .# unTagged
  {-# INLINE fmap #-}

instance Bifunctor Tagged where
  bimap _f g = Tagged #. g .# unTagged
  {-# INLINE bimap #-}

instance Profunctor Tagged where
  dimap _f g = Tagged #. g .# unTagged
  {-# INLINE dimap #-}

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
