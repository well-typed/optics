-- |
-- Module: Optics.Operators
-- Description: Definitions of infix operators for optics.
--
-- Defines some infix operators for optics operations. This is a deliberately
-- small collection.
--
-- If you like operators, you may also wish to import @Optics.State.Operators@
-- from the @optics-extra@ package.
--
module Optics.Operators
  ( (^.)
  , (^..)
  , (^?)
  , (#)
  , (%~)
  , (%!~)
  , (.~)
  , (!~)
  , (?~)
  , (?!~)
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

-- | Infix version of 'over''.
(%!~) :: Is k A_Setter => Optic k is s t a b -> (a -> b) -> s -> t
(%!~) = over'
{-# INLINE (%!~) #-}

infixr 4 %!~

-- | Infix version of 'set'.
(.~) :: Is k A_Setter => Optic k is s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

infixr 4 .~

-- | Infix version of 'set''.
(!~) :: Is k A_Setter => Optic k is s t a b -> b -> s -> t
(!~) = set'
{-# INLINE (!~) #-}

infixr 4 !~

-- | Set the target of a 'Setter' to 'Just' a value.
--
-- @
-- o '?~' b ≡ 'set' o ('Just' b)
-- @
--
-- >>> Nothing & equality ?~ 'x'
-- Just 'x'
--
-- >>> Map.empty & at 3 ?~ 'x'
-- fromList [(3,'x')]
(?~) :: Is k A_Setter => Optic k is s t a (Maybe b) -> b -> s -> t
(?~) = \o -> set o . Just
{-# INLINE (?~) #-}

infixr 4 ?~

-- | Strict version of ('?~').
(?!~) :: Is k A_Setter => Optic k is s t a (Maybe b) -> b -> s -> t
(?!~) = \o !b -> set' o (Just b)
{-# INLINE (?!~) #-}

infixr 4 ?!~

-- $setup
-- >>> import qualified Data.Map as Map
-- >>> import Optics.Core
