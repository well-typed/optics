{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Utils
  ( Box
  , wrapBox
  , unwrapBox

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

-- Needed for strict application of (indexed) traversals.
--
-- Credit for this goes to Eric Mertens, see
-- <https://github.com/glguy/irc-core/commit/2d5fc45b05f1>.
--
-- Both the data type and the laziness of its field are necessary:
--
-- - It must not be a newtype, as then matching on the constructor in '<*>' and
--   'unwrapBox' would no longer force the value put in by 'wrapBox'.
--
-- - The field must stay lazy, with only 'wrapBox' forcing what it puts in it.
--   A strict field would make 'pure' and 'fmap' force the reconstructed
--   structure as well, not only the new values.
data Box a = Box a
  deriving Functor

instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)

-- | Mark a value for evaluation to WHNF.
--
-- This allows us to, when applying a traversal to a structure, evaluate only
-- the parts that we modify. If an optic focuses on multiple targets,
-- Applicative instance of Box makes sure that we force evaluation of all of
-- them, but we leave anything else alone.
--
wrapBox :: a -> Box a
wrapBox a = Box $! a

unwrapBox :: Box a -> a
unwrapBox (Box a) = a

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
