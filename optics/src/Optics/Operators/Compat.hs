{-# LANGUAGE FlexibleContexts #-}
module Optics.Operators.Compat where

import Optics.Fold

-- | Flipped infix version of 'toListOf'.
(^..) :: Is k A_Fold => s -> Optic' k is s a -> [a]
(^..) = flip toListOf
{-# INLINE (^..) #-}

infixl 8 ^..

-- | Flipped infix version of 'preview'.
(^?) :: Is k A_Fold => s -> Optic' k is s a -> Maybe a
(^?) = flip preview
{-# INLINE (^?) #-}

infixl 8 ^?
