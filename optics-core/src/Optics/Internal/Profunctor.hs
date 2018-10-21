{-# LANGUAGE RankNTypes #-}

-- | Copied from the profunctors package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Profunctor where

import Data.Functor.Const
import Data.Functor.Identity

-- | Needed for traversals.
newtype Star f a b = Star { runStar :: a -> f b }

-- | Needed for prismatic getters, getters and folds.
newtype Forget r a b = Forget { runForget :: a -> r }

-- | Needed for affine folds.
newtype ForgetM r a b = ForgetM { runForgetM :: a -> Maybe r }

----------------------------------------

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap f g k = g . k . f
  {-# INLINE dimap #-}

instance Functor f => Profunctor (Star f) where
  dimap f g (Star k) = Star (fmap g . k . f)
  {-# INLINE dimap #-}

instance Profunctor (Forget r) where
  dimap f _ (Forget k) = Forget (k . f)
  {-# INLINE dimap #-}

instance Profunctor (ForgetM r) where
  dimap f _ (ForgetM k) = ForgetM (k . f)
  {-# INLINE dimap #-}

----------------------------------------

class Profunctor p => Strong p where
  first'  :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

instance Strong (->) where
  first'  f ~(a, c) = (f a, c)
  second' f ~(c, a) = (c, f a)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Functor f => Strong (Star f) where
  first'  (Star f) = Star $ \ ~(a, c) -> (\b' -> (b', c)) <$> f a
  second' (Star f) = Star $ \ ~(c, a) -> (,) c <$> f a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Strong (Forget r) where
  first'  (Forget k) = Forget (k . fst)
  second' (Forget k) = Forget (k . snd)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Strong (ForgetM r) where
  first'  (ForgetM k) = ForgetM (k . fst)
  second' (ForgetM k) = ForgetM (k . snd)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

----------------------------------------

class Profunctor p => Costrong p where
  unfirst  :: p (a, d) (b, d) -> p a b
  unsecond :: p (d, a) (d, b) -> p a b

----------------------------------------

class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  left' f = either (Left . f) Right
  right'  = fmap
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Applicative f => Choice (Star f) where
  left'  (Star f) = Star $ either (fmap Left . f) (pure . Right)
  right' (Star f) = Star $ either (pure . Left) (fmap Right . f)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Monoid r => Choice (Forget r) where
  left'  (Forget k) = Forget (either k (const mempty))
  right' (Forget k) = Forget (either (const mempty) k)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Choice (ForgetM r) where
  left'  (ForgetM k) = ForgetM (either k (const Nothing))
  right' (ForgetM k) = ForgetM (either (const Nothing) k)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

----------------------------------------

class Profunctor p => Cochoice p where
  unleft  :: p (Either a d) (Either b d) -> p a b
  unright :: p (Either d a) (Either d b) -> p a b

instance Cochoice (Forget r) where
  unleft  (Forget k) = Forget (k . Left)
  unright (Forget k) = Forget (k . Right)
  {-# INLINE unleft #-}
  {-# INLINE unright #-}

instance Cochoice (ForgetM r) where
  unleft  (ForgetM k) = ForgetM (k . Left)
  unright (ForgetM k) = ForgetM (k . Right)
  {-# INLINE unleft #-}
  {-# INLINE unright #-}

----------------------------------------

class (Choice p, Strong p) => Traversing p where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t)
         -> p a b -> p s t

instance Traversing (->) where
  wander t f = runIdentity . t (Identity . f)
  {-# INLINE wander #-}

instance Applicative f => Traversing (Star f) where
  wander t (Star f) = Star (t f)
  {-# INLINE wander #-}

instance Monoid r => Traversing (Forget r) where
  wander f (Forget r) = Forget (getConst . f (Const . r))
  {-# INLINE wander #-}
