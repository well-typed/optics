{-# LANGUAGE FlexibleContexts #-}

-- | Defines some infix operators for optics operations.
--
-- These are not exported by default from 'Optics'.
-- They have to be imported separately.
--
module Optics.Operators
  ( (&)
  , (^.)
  , (^..)
  , (^?)
  , (#)
  , (%~)
  , (.~)
  )
  where

import Data.Function

import Optics

-- | Flipped infix version of 'view'.
(^.) :: Is k A_Getter => s -> Optic' k s a -> a
(^.) = flip view
{-# INLINE (^.) #-}

infixl 8 ^.

-- | Flipped infix version of 'toListOf'.
(^..) :: Is k A_Fold => s -> Optic' k s a -> [a]
(^..) = flip toListOf
{-# INLINE (^..) #-}

infixl 8 ^..

-- | Flipped infix version of 'preview'.
(^?) :: Is k A_Fold => s -> Optic' k s a -> Maybe a
(^?) = flip preview
{-# INLINE (^?) #-}

infixl 8 ^?

-- | Flipped infix version of 'review'.
(#) :: Is k A_Review => b -> Optic' k t b -> t
(#) = flip review
{-# INLINE (#) #-}
infixr 8 #

-- | Infix version of 'over'.
(%~) :: Is k A_Setter => Optic k s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infixr 4 %~

-- | Infix version of 'set'.
(.~) :: Is k A_Setter => Optic k s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~
