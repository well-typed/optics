module Optics.Core.Extras
  (
    is
  )
  where

import Data.Maybe

import Optics.Optic
import Optics.AffineFold

-- | Check to see if this 'AffineFold' matches.
--
-- >>> is _Just Nothing
-- False
--
-- @since 0.4.1
is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k s = isJust (preview k s)
{-# INLINE is #-}

-- $setup
-- >>> import Optics.Core
