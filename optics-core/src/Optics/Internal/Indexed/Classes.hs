{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of indexed optics.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Indexed.Classes (
   module Data.Functor.WithIndex,
   module Data.Foldable.WithIndex,
   module Data.Traversable.WithIndex,
) where

import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex
