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

import Optics.AffineTraversal

-- | An 'AffineTraversal'' for working with a 'cast' of a 'Typeable' value.
_cast :: (Typeable s, Typeable a) => AffineTraversal' s a
_cast = atraversalVL $ \point f s -> case cast s of
  Just a  -> fromMaybe (error "_cast: recast failed") . cast <$> f a
  Nothing -> point s
{-# INLINE _cast #-}

-- | An 'AffineTraversal'' for working with a 'gcast' of a 'Typeable' value.
_gcast :: (Typeable s, Typeable a) => AffineTraversal' (c s) (c a)
_gcast = atraversalVL $ \point f s -> case gcast s of
  Just a  -> fromMaybe (error "_gcast: recast failed") . gcast <$> f a
  Nothing -> point s
{-# INLINE _gcast #-}
