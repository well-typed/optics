{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Utils
  ( Identity'(..)
  , wrapIdentity'
  , unwrapIdentity'

  , Traversed(..)
  , runTraversed

  , OrT(..)
  , wrapOrT

  , (#.)
  , (.#)
  , uncurry'
  ) where

import qualified Data.Semigroup as SG

import Data.Profunctor.Indexed

-- Needed for strict application of (indexed) setters.
--
-- Credit for this goes to Eric Mertens, see
-- <https://github.com/glguy/irc-core/commit/2d5fc45b05f1>.
data Identity' a = Identity' {-# UNPACK #-} !() a
  deriving Functor

instance Applicative Identity' where
  pure a = Identity' () a
  Identity' () f <*> Identity' () x = Identity' () (f x)

instance Mapping (Star Identity') where
  roam  f (Star k) = Star $ wrapIdentity' . f (unwrapIdentity' . k)
  iroam f (Star k) = Star $ wrapIdentity' . f (\_ -> unwrapIdentity' . k)

instance Mapping (IxStar Identity') where
  roam  f (IxStar k) =
    IxStar $ \i -> wrapIdentity' . f (unwrapIdentity' . k i)
  iroam f (IxStar k) =
    IxStar $ \ij -> wrapIdentity' . f (\i -> unwrapIdentity' . k (ij i))

-- | Mark a value for evaluation to whnf.
--
-- This allows us to, when applying a setter to a structure, evaluate only the
-- parts that we modify. If an optic focuses on multiple targets, Applicative
-- instance of Identity' makes sure that we force evaluation of all of them, but
-- we leave anything else alone.
--
wrapIdentity' :: a -> Identity' a
wrapIdentity' a = Identity' (a `seq` ()) a

unwrapIdentity' :: Identity' a -> a
unwrapIdentity' (Identity' () a) = a

----------------------------------------

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
