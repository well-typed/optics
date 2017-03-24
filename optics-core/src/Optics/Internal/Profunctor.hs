{-# LANGUAGE KindSignatures #-}

-- | Copied from the profunctors package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Profunctor where

import Control.Applicative (Const(..))
import Data.Bifunctor

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant (Const r) where
  contramap _ (Const x) = Const x

class Profunctor (p :: * -> * -> *) where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap f g k = g . k . f

class Profunctor p => Choice (p :: * -> * -> *) where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)

class Profunctor p => Strong (p :: * -> * -> *) where
  first'  :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

instance Strong (->) where
  first'  f = bimap f id
  second' f = bimap id f

-- This is taken from the profunctor optics paper
-- (but not currently being used):

class Profunctor p => Monoidal p where
  empty :: p () ()
  par   :: p a b -> p c d -> p (a, c) (b, d)

instance Monoidal (->) where
  empty     = id
  par ab cd = bimap ab cd

-- These are taken from mezzolens:

class Strong p => OutPhantom p where
  ocoerce :: p c a -> p c b

class Choice p => InPhantom p where
  icoerce :: p a c -> p b c

class (Strong p, Choice p) => Wandering p where
  wander :: Traversable f => p a b -> p (f a) (f b)

instance Wandering (->) where
  wander = fmap
