{-# LANGUAGE FlexibleContexts #-}
module Optics.Prism
  ( A_Prism
  , Prism
  , Prism'
  , toPrism
  , prism
  , withPrism
  , aside
  , without
  , below
  , isn't
  , matching
  , module Optics.Optic
  )
  where

import Data.Bifunctor

import Optics.Internal.Prism
import Optics.Optic

-- | Use a 'Prism' to work over part of a structure.
aside :: Is k A_Prism => Optic k s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside k =
  withPrism k     $ \bt seta ->
  prism (fmap bt) $ \(e,s) ->
  case seta s of
    Left t  -> Left  (e,t)
    Right a -> Right (e,a)
{-# INLINE aside #-}

-- | Given a pair of prisms, project sums.
without
  :: (Is k A_Prism, Is l A_Prism)
  => Optic k s t a b
  -> Optic l u v c d
  -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without k =
  withPrism k         $ \bt seta k' ->
  withPrism k'        $ \dv uevc    ->
  prism (bimap bt dv) $ \su ->
  case su of
    Left s  -> bimap Left Left (seta s)
    Right u -> bimap Right Right (uevc u)
{-# INLINE without #-}

-- | 'lift' a 'Prism' through a 'Traversable' functor, giving a Prism that
-- matches only if all the elements of the container match the 'Prism'.
below :: (Is k A_Prism, Traversable f) => Optic' k s a -> Prism' (f s) (f a)
below k =
  withPrism k     $ \bt seta ->
  prism (fmap bt) $ \s ->
  case traverse seta s of
    Left _  -> Left s
    Right t -> Right t
{-# INLINE below #-}

-- | Check to see if this 'Prism' doesn't match.
isn't :: Is k A_Prism => Optic k s t a b -> s -> Bool
isn't k s =
  case matching k s of
    Left  _ -> True
    Right _ -> False
{-# INLINE isn't #-}

-- | Retrieve the value targeted by a 'Prism' or return the original value while
-- allowing the type to change if it does not match.
matching :: Is k A_Prism => Optic k s t a b -> s -> Either t a
matching o = withPrism o $ \_ match -> match
{-# INLINE matching #-}
