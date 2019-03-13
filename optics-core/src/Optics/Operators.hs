-- | Defines some infix operators for optics operations.
--
-- These are not exported by default from "Optics.Core".
-- They have to be imported separately.
--
module Optics.Operators
  ( (^.)
  , (^..)
  , (^?)
  , (#)
  , (%~)
  , (.~)
  )
  where

import Optics.AffineFold
import Optics.Fold
import Optics.Getter
import Optics.Review
import Optics.Setter

-- | Flipped infix version of 'view'.
(^.) :: Is k A_Getter => s -> Optic' k is s a -> a
(^.) = flip view
{-# INLINE (^.) #-}

infixl 8 ^.

-- | Flipped infix version of 'preview'.
(^?) :: Is k An_AffineFold => s -> Optic' k is s a -> Maybe a
(^?) = flip preview
{-# INLINE (^?) #-}

infixl 8 ^?

-- | Flipped infix version of 'toListOf'.
(^..) :: Is k A_Fold => s -> Optic' k is s a -> [a]
(^..) = flip toListOf
{-# INLINE (^..) #-}

infixl 8 ^..

-- | Flipped infix version of 'review'.
(#) :: Is k A_Review => Optic' k is t b -> b -> t
(#) = review
{-# INLINE (#) #-}
infixr 8 #

-- | Infix version of 'over'.
(%~) :: Is k A_Setter => Optic k is s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

infixr 4 %~

-- | Infix version of 'set'.
(.~) :: Is k A_Setter => Optic k is s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~
