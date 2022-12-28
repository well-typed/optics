{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Utils
  ( Traversed(..)
  , runTraversed

  , OrT(..)
  , wrapOrT

  , (#.)
  , (.#)
  , uncurry'
  ) where

import qualified Data.Semigroup as SG

import Data.Profunctor.Indexed

-- | Helper for 'Optics.Fold.traverseOf_' and the like for better
-- efficiency than the foldr-based version.
--
-- Note that the argument @a@ of the result should not be used.
newtype Traversed f a = Traversed (f a)

runTraversed :: Functor f => Traversed f a -> f ()
runTraversed (Traversed fa) = () <$ fa

instance Applicative f => SG.Semigroup (Traversed f a) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)

instance Applicative f => Monoid (Traversed f a) where
  mempty = Traversed (pure (error "Traversed: value used"))
  mappend = (SG.<>)

----------------------------------------

-- | Helper for 'Optics.Fold.failing' family to visit the first fold only once.
data OrT f a = OrT !Bool (f a)
  deriving Functor

instance Applicative f => Applicative (OrT f) where
  pure = OrT False . pure
  OrT a f <*> OrT b x = OrT (a || b) (f <*> x)

-- | Wrap the applicative action in 'OrT' so that we know later that it was
-- executed.
wrapOrT :: f a -> OrT f a
wrapOrT = OrT True

-- | 'uncurry' with no lazy pattern matching for more efficient code.
--
-- @since 0.3
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b
