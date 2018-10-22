{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Prism where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying prism.
type Prism s t a b = Optic A_Prism s t a b

-- | Type synonym for a type-preserving prism.
type Prism' s a = Optic' A_Prism s a

-- | Explicitly cast an optic to a prism.
toPrism :: Is k A_Prism => Optic k s t a b -> Prism s t a b
toPrism = sub
{-# INLINE toPrism #-}

-- | Build a prism from a constructor and a matcher.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism construct match = Optic $ dimap match (either id construct) . right'
{-# INLINE prism #-}

withPrism
  :: Is k A_Prism
  => Optic k s t a b
  -> ((b -> t) -> (s -> Either t a) -> r)
  -> r
withPrism o k = case getOptic (toPrism o) (Market id Right) of
  Market construct match -> k construct match
{-# INLINE withPrism #-}

----------------------------------------

-- | Type to represent the components of a prism.
data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE left' #-}
  {-# INLINE right' #-}
