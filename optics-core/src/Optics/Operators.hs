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
(^.) :: ViewableOptic k a => s -> Optic' k i i s a -> ViewResult k a
(^.) = flip view
{-# INLINE (^.) #-}

infixl 8 ^.

-- | Flipped infix version of 'toListOf'.
(^..) :: Is k A_Fold => s -> Optic' k i i s a -> [a]
(^..) = flip toListOf
{-# INLINE (^..) #-}

infixl 8 ^..

-- | Flipped infix version of 'preview'.
(^?) :: Is k A_Fold => s -> Optic' k i i s a -> Maybe a
(^?) = flip preview
{-# INLINE (^?) #-}

infixl 8 ^?

-- | Flipped infix version of 'review'.
(#) :: Is k A_Review => b -> Optic' k i i t b -> t
(#) = flip review
{-# INLINE (#) #-}
infixr 8 #

-- | Infix version of 'over'.
(%~) :: Is k A_Setter => Optic k i i s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infixr 4 %~

-- | Infix version of 'set'.
(.~) :: Is k A_Setter => Optic k i i s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~
