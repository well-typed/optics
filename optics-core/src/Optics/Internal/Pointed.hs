module Optics.Internal.Pointed where

import Data.Functor.Const
import Data.Functor.Identity

class Functor f => Pointed f where
    point :: a -> f a
    default point :: Applicative f => a -> f a
    point = pure

instance Pointed Identity
instance Monoid b => Pointed (Const b)
instance Pointed (Either b)

newtype ConstM b a = ConstM { getConstM :: Maybe b }
  deriving Functor

instance Pointed (ConstM m) where point _ = ConstM Nothing

newtype WrappedPointed f a = WrapPointed { unwrapPointed :: f a }
  deriving Functor

instance Applicative f => Pointed (WrappedPointed f) where
    point = WrapPointed . pure
