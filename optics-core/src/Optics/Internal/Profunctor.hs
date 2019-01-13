-- | Copied from the profunctors package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Profunctor where

import Data.Functor.Const
import Data.Functor.Identity

import Optics.Internal.Utils

-- | Needed for traversals.
newtype Star f i a b = Star { runStar :: a -> f b }

-- | Needed for prismatic getters, getters and folds.
newtype Forget r i a b = Forget { runForget :: a -> r }

-- | Needed for affine folds.
newtype ForgetM r i a b = ForgetM { runForgetM :: a -> Maybe r }

-- | Needed for setters.
newtype FunArrow i a b = FunArrow { runFunArrow :: a -> b }

-- | Needed for indexed traversals.
newtype IxStar f i a b = IxStar { runIxStar :: i -> a -> f b }

-- | Needed for indexed folds.
newtype IxForget r i a b = IxForget { runIxForget :: i -> a -> r }

-- | Needed for indexed setters.
newtype IxFunArrow i a b = IxFunArrow { runIxFunArrow :: i -> a -> b }

----------------------------------------

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p i b c -> p i a d

instance Functor f => Profunctor (Star f) where
  dimap f g (Star k) = Star (fmap g . k . f)
  {-# INLINE dimap #-}

instance Profunctor (Forget r) where
  dimap f _ (Forget k) = Forget (k . f)
  {-# INLINE dimap #-}

instance Profunctor (ForgetM r) where
  dimap f _ (ForgetM k) = ForgetM (k . f)
  {-# INLINE dimap #-}

instance Profunctor FunArrow where
  dimap f g (FunArrow k) = FunArrow (g . k . f)
  {-# INLINE dimap #-}

instance Functor f => Profunctor (IxStar f) where
  dimap f g (IxStar k) = IxStar (\i -> fmap g . k i . f)
  {-# INLINE dimap #-}

instance Profunctor (IxForget r) where
  dimap f _ (IxForget k) = IxForget (\i -> k i . f)
  {-# INLINE dimap #-}

instance Profunctor IxFunArrow where
  dimap f g (IxFunArrow k) = IxFunArrow (\i -> g . k i . f)
  {-# INLINE dimap #-}

----------------------------------------

class Profunctor p => Strong p where
  first'  :: p i a b -> p i (a, c) (b, c)
  second' :: p i a b -> p i (c, a) (c, b)

instance Functor f => Strong (Star f) where
  first'  (Star k) = Star $ \ ~(a, c) -> (\b' -> (b', c)) <$> k a
  second' (Star k) = Star $ \ ~(c, a) -> (,) c <$> k a
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

instance Strong FunArrow where
  first'  (FunArrow k) = FunArrow $ \ ~(a, c) -> (k a, c)
  second' (FunArrow k) = FunArrow $ \ ~(c, a) -> (c, k a)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Functor f => Strong (IxStar f) where
  first'  (IxStar k) = IxStar $ \i ~(a, c) -> (\b' -> (b', c)) <$> k i a
  second' (IxStar k) = IxStar $ \i ~(c, a) -> (,) c <$> k i a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Strong (IxForget r) where
  first'  (IxForget k) = IxForget $ \i -> k i . fst
  second' (IxForget k) = IxForget $ \i -> k i . snd
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Strong IxFunArrow where
  first'  (IxFunArrow k) = IxFunArrow $ \i ~(a, c) -> (k i a, c)
  second' (IxFunArrow k) = IxFunArrow $ \i ~(c, a) -> (c, k i a)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

----------------------------------------

class Profunctor p => Costrong p where
  unfirst  :: p i (a, d) (b, d) -> p i a b
  unsecond :: p i (d, a) (d, b) -> p i a b

----------------------------------------

class Profunctor p => Choice p where
  left'  :: p i a b -> p i (Either a c) (Either b c)
  right' :: p i a b -> p i (Either c a) (Either c b)

instance Applicative f => Choice (Star f) where
  left'  (Star k) = Star $ either (fmap Left . k) (pure . Right)
  right' (Star k) = Star $ either (pure . Left) (fmap Right . k)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Monoid r => Choice (Forget r) where
  left'  (Forget k) = Forget $ either k (const mempty)
  right' (Forget k) = Forget $ either (const mempty) k
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Choice (ForgetM r) where
  left'  (ForgetM k) = ForgetM $ either k (const Nothing)
  right' (ForgetM k) = ForgetM $ either (const Nothing) k
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Choice FunArrow where
  left'  (FunArrow k) = FunArrow $ either (Left . k) Right
  right' (FunArrow k) = FunArrow $ either Left (Right . k)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Applicative f => Choice (IxStar f) where
  left'  (IxStar k) = IxStar $ \i -> either (fmap Left . k i) (pure . Right)
  right' (IxStar k) = IxStar $ \i -> either (pure . Left) (fmap Right . k i)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Monoid r => Choice (IxForget r) where
  left'  (IxForget k) = IxForget $ \i -> either (k i) (const mempty)
  right' (IxForget k) = IxForget $ \i -> either (const mempty) (k i)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Choice IxFunArrow where
  left'  (IxFunArrow k) = IxFunArrow $ \i -> either (Left . k i) Right
  right' (IxFunArrow k) = IxFunArrow $ \i -> either Left (Right . k i)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

----------------------------------------

class Profunctor p => Cochoice p where
  unleft  :: p i (Either a d) (Either b d) -> p i a b
  unright :: p i (Either d a) (Either d b) -> p i a b

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

instance Cochoice (IxForget r) where
  unleft  (IxForget k) = IxForget $ \i -> k i . Left
  unright (IxForget k) = IxForget $ \i -> k i . Right
  {-# INLINE unleft #-}
  {-# INLINE unright #-}

----------------------------------------

class (Choice p, Strong p) => Traversing p where
  wander
    :: (forall f. Applicative f => (a -> f b) -> s -> f t)
    -> p i a b -> p i s t

instance Applicative f => Traversing (Star f) where
  wander f (Star k) = Star (f k)
  {-# INLINE wander #-}

instance Monoid r => Traversing (Forget r) where
  wander f (Forget k) = Forget $ getConst #. f (Const #. k)
  {-# INLINE wander #-}

instance Traversing FunArrow where
  wander f (FunArrow k) = FunArrow $ runIdentity #. f (Identity #. k)
  {-# INLINE wander #-}

instance Applicative f => Traversing (IxStar f) where
  wander f (IxStar k) = IxStar $ \i -> f (k i)
  {-# INLINE wander #-}

instance Monoid r => Traversing (IxForget r) where
  wander f (IxForget k) = IxForget $ \i -> getConst #. f (Const #. k i)
  {-# INLINE wander #-}

instance Traversing IxFunArrow where
  wander f (IxFunArrow k) = IxFunArrow $ \i -> runIdentity #. f (Identity #. k i)
  {-# INLINE wander #-}

----------------------------------------

class Traversing p => Mapping p where
  roam :: ((a -> b) -> s -> t) -> p i a b -> p i s t

instance Mapping FunArrow where
  roam f (FunArrow k) = FunArrow (f k)
  {-# INLINE roam #-}

instance Mapping IxFunArrow where
  roam f (IxFunArrow k) = IxFunArrow $ \i -> f (k i)
  {-# INLINE roam #-}

----------------------------------------

class IxProfunctor p where
  ixcontramap :: (i -> j) -> p j a b -> p i a b

instance IxProfunctor (Star f) where
  ixcontramap _ij (Star k) = Star k
  {-# INLINE ixcontramap #-}

instance IxProfunctor (Forget r) where
  ixcontramap _ij (Forget k) = Forget k
  {-# INLINE ixcontramap #-}

instance IxProfunctor FunArrow where
  ixcontramap _ij (FunArrow k) = FunArrow k
  {-# INLINE ixcontramap #-}

instance IxProfunctor (IxStar f) where
  ixcontramap ij (IxStar k) = IxStar $ \i -> k (ij i)
  {-# INLINE ixcontramap #-}

instance IxProfunctor (IxForget r) where
  ixcontramap ij (IxForget k) = IxForget $ \i -> k (ij i)
  {-# INLINE ixcontramap #-}

instance IxProfunctor IxFunArrow where
  ixcontramap ij (IxFunArrow k) = IxFunArrow $ \i -> k (ij i)
  {-# INLINE ixcontramap #-}

----------------------------------------

class (IxProfunctor p, Traversing p) => IxTraversing p where
  iwander
    :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
    -> p j a b -> p (i -> j) s t

instance Applicative f => IxTraversing (Star f) where
  iwander f (Star k) = Star $ f (\_ -> k)
  {-# INLINE iwander #-}

instance Monoid r => IxTraversing (Forget r) where
  iwander f (Forget k) = Forget $ getConst #. f (\_ -> Const #. k)
  {-# INLINE iwander #-}

instance IxTraversing FunArrow where
  iwander f (FunArrow k) =
    FunArrow $ runIdentity #. f (\_ -> Identity #. k)
  {-# INLINE iwander #-}

instance Applicative f => IxTraversing (IxStar f) where
  iwander f (IxStar k) = IxStar $ \ij -> f $ \i -> k (ij i)
  {-# INLINE iwander #-}

instance Monoid r => IxTraversing (IxForget r) where
  iwander f (IxForget k) =
    IxForget $ \ij -> getConst #. f (\i -> Const #. k (ij i))
  {-# INLINE iwander #-}

instance IxTraversing IxFunArrow where
  iwander f (IxFunArrow k) =
    IxFunArrow $ \ij -> runIdentity #. f (\i -> Identity #. k (ij i))
  {-# INLINE iwander #-}

----------------------------------------

class (IxTraversing p, Mapping p) => IxMapping p where
  iroam :: ((i -> a -> b) -> s -> t) -> p j a b -> p (i -> j) s t

instance IxMapping FunArrow where
  iroam f (FunArrow k) = FunArrow (f (const k))

instance IxMapping IxFunArrow where
  iroam f (IxFunArrow k) =
    IxFunArrow $ \ij -> f $ \i -> k (ij i)
