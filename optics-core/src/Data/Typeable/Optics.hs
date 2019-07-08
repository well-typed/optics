-- |
-- Module: Data.Typeable.Optics
-- Description: Optics for working with 'Typeable'.
--
module Data.Typeable.Optics
  ( _cast
  , _gcast
  ) where

import Data.Typeable
import Data.Maybe

import Optics.Traversal

-- | A 'Traversal'' for working with a 'cast' of a 'Typeable' value.
_cast :: (Typeable s, Typeable a) => Traversal' s a
_cast = traversalVL $ \f s -> case cast s of
  Just a  -> fromMaybe (error "_cast: recast failed") . cast <$> f a
  Nothing -> pure s
{-# INLINE _cast #-}

-- | A 'Traversal'' for working with a 'gcast' of a 'Typeable' value.
_gcast :: (Typeable s, Typeable a) => Traversal' (c s) (c a)
_gcast = traversalVL $ \f s -> case gcast s of
  Just a  -> fromMaybe (error "_gcast: recast failed") . gcast <$> f a
  Nothing -> pure s
{-# INLINE _gcast #-}
