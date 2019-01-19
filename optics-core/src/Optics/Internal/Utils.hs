module Optics.Internal.Utils where

import Data.Coerce
import qualified Data.Semigroup as SG

data Context a b t = Context (b -> t) a
  deriving Functor

data IxContext i a b t = IxContext (i -> b -> t) a
  deriving Functor

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
infixl 8 .#
{-# INLINE (#.) #-}

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _g = coerce f
infixr 9 #.
{-# INLINE (.#) #-}

-- | Helper for 'traverseOf_' and the like for better efficiency than the
-- foldrOf-based version.
--
-- Note that the argument 'a' of the result should not be used.
newtype Traversed f a = Traversed (f a)

runTraversed :: Functor f => Traversed f a -> f ()
runTraversed (Traversed fa) = () <$ fa
{-# INLINE runTraversed #-}

instance Applicative f => SG.Semigroup (Traversed f a) where
  Traversed ma <> Traversed mb = Traversed (ma *> mb)
  {-# INLINE (<>) #-}

instance Applicative f => Monoid (Traversed f a) where
  mempty = Traversed (pure (error "Traversed: value used"))
  Traversed ma `mappend` Traversed mb = Traversed (ma *> mb)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
