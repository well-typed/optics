{-# LANGUAGE KindSignatures #-}

-- | Copied from the profunctors package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Profunctor where

import Control.Applicative (Const(..))

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant (Const r) where
  contramap _ (Const x) = Const x

class Profunctor (p :: * -> * -> *) where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap f g k = g . k . f

class Profunctor p => Choice (p :: * -> * -> *) where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)
