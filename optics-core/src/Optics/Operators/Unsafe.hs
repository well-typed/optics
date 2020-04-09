-- |
-- Module: Optics.Operators.Unsafe
-- Description: Definitions of unsafe infix operators for optics.
--
module Optics.Operators.Unsafe
  ( (^?!)
  )
  where

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

import Optics.AffineFold
import Optics.Optic
import Optics.Operators

-- | Perform an *UNSAFE* 'head' of an affine fold assuming that it is there.
--
-- >>> Left 4 ^?! _Left
-- 4
--
-- >>> "world" ^?! ix 3
-- 'l'
--
-- >>> [] ^?! _head
-- *** Exception: (^?!): empty affine fold
-- ...
--
-- @since 0.3
(^?!) :: (HasCallStack, Is k An_AffineFold) => s -> Optic' k is s a -> a
s ^?! o = fromMaybe (error "(^?!): empty affine fold") (s ^? o)
{-# INLINE (^?!) #-}

infixl 8 ^?!

-- $setup
-- >>> import Optics.Core
