{-# LANGUAGE CPP #-}
-- | Defines some infix operators for optics operations.
--
-- These are not exported by default from 'Optics'.
-- They have to be imported separately.
--
module Optics.Operators
  ( (&)
  , (<&>)
  , (^.)
  , (^..)
  , (^?)
  , (#)
  , (%~)
  , (.~)
  )
  where

import Data.Function

import Optics.Fold
import Optics.Review
import Optics.Setter
import Optics.View

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#else
-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
#endif

-- | Flipped infix version of 'view'.
(^.) :: ViewableOptic k is a => s -> Optic' k is s a -> ViewResult k is a
(^.) = flip view
{-# INLINE (^.) #-}

infixl 8 ^.

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

-- | Infix version of 'review'.
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
