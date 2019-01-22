-- | Copied from the profunctors package.
--
-- We include this here, at least for now, with the goal
-- that we only depend on base.
--
module Optics.Internal.Profunctor where

import Data.Coerce (Coercible, coerce)
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
  lmap  :: (a -> b)             -> p i b c -> p i a c
  rmap  ::             (c -> d) -> p i b c -> p i b d

  lcoerce' :: Coercible a b => p i a c -> p i b c
  default lcoerce'
    :: Coercible (p i a c) (p i b c)
    => p i a c
    -> p i b c
  lcoerce' = coerce
  {-# INLINE lcoerce' #-}

  rcoerce' :: Coercible a b => p i c a -> p i c b
  default rcoerce'
    :: Coercible (p i c a) (p i c b)
    => p i c a
    -> p i c b
  rcoerce' = coerce
  {-# INLINE rcoerce' #-}

-- | 'rcoerce'' with type arguments rearranged for TypeApplications.
rcoerce :: (Coercible a b, Profunctor p) => p i c a -> p i c b
rcoerce = rcoerce'
{-# INLINE rcoerce #-}

-- | 'lcoerce'' with type arguments rearranged for TypeApplications.
lcoerce :: (Coercible a b, Profunctor p) => p i a c -> p i b c
lcoerce = lcoerce'
{-# INLINE lcoerce #-}

instance Functor f => Profunctor (Star f) where
  dimap f g (Star k) = Star (fmap g . k . f)
  lmap  f   (Star k) = Star (k . f)
  rmap    g (Star k) = Star (fmap g . k)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  rcoerce' = rmap coerce
  {-# INLINE rcoerce' #-}

instance Profunctor (Forget r) where
  dimap f _ (Forget k) = Forget (k . f)
  lmap  f   (Forget k) = Forget (k . f)
  rmap   _g (Forget k) = Forget k
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Profunctor (ForgetM r) where
  dimap f _ (ForgetM k) = ForgetM (k . f)
  lmap  f   (ForgetM k) = ForgetM (k . f)
  rmap   _g (ForgetM k) = ForgetM k
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Profunctor FunArrow where
  dimap f g (FunArrow k) = FunArrow (g . k . f)
  lmap  f   (FunArrow k) = FunArrow (k . f)
  rmap    g (FunArrow k) = FunArrow (g . k)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Functor f => Profunctor (IxStar f) where
  dimap f g (IxStar k) = IxStar (\i -> fmap g . k i . f)
  lmap  f   (IxStar k) = IxStar (\i -> k i . f)
  rmap    g (IxStar k) = IxStar (\i -> fmap g . k i)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  rcoerce' = rmap coerce
  {-# INLINE rcoerce' #-}

instance Profunctor (IxForget r) where
  dimap f _ (IxForget k) = IxForget (\i -> k i . f)
  lmap  f   (IxForget k) = IxForget (\i -> k i . f)
  rmap   _g (IxForget k) = IxForget k
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Profunctor IxFunArrow where
  dimap f g (IxFunArrow k) = IxFunArrow (\i -> g . k i . f)
  lmap  f   (IxFunArrow k) = IxFunArrow (\i -> k i . f)
  rmap    g (IxFunArrow k) = IxFunArrow (\i -> g . k i)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

----------------------------------------

class Profunctor p => Strong p where
  first'  :: p i a b -> p i (a, c) (b, c)
  second' :: p i a b -> p i (c, a) (c, b)

  -- There are few places where default implementation is good enough.
  linear  :: (forall f. Functor f => (a -> f b) -> s -> f t) -> p i a b -> p i s t
  linear f = dimap
    ((\(Context bt a) -> (a, bt)) . f (Context id))
    (\(b, bt) -> bt b)
    . first'
  {-# INLINE linear #-}

instance Functor f => Strong (Star f) where
  first'  (Star k) = Star $ \ ~(a, c) -> (\b' -> (b', c)) <$> k a
  second' (Star k) = Star $ \ ~(c, a) -> (,) c <$> k a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (Star k) = Star (f k)
  {-# INLINE linear #-}

instance Strong (Forget r) where
  first'  (Forget k) = Forget (k . fst)
  second' (Forget k) = Forget (k . snd)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (Forget k) = Forget (getConst #. f (Const #. k))
  {-# INLINE linear #-}

instance Strong (ForgetM r) where
  first'  (ForgetM k) = ForgetM (k . fst)
  second' (ForgetM k) = ForgetM (k . snd)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (ForgetM k) = ForgetM (getConst #. f (Const #. k))
  {-# INLINE linear #-}

instance Strong FunArrow where
  first'  (FunArrow k) = FunArrow $ \ ~(a, c) -> (k a, c)
  second' (FunArrow k) = FunArrow $ \ ~(c, a) -> (c, k a)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (FunArrow k) = FunArrow $ runIdentity #. f (Identity #. k)
  {-# INLINE linear #-}

instance Functor f => Strong (IxStar f) where
  first'  (IxStar k) = IxStar $ \i ~(a, c) -> (\b' -> (b', c)) <$> k i a
  second' (IxStar k) = IxStar $ \i ~(c, a) -> (,) c <$> k i a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxStar k) = IxStar $ \i -> f (k i)
  {-# INLINE linear #-}

instance Strong (IxForget r) where
  first'  (IxForget k) = IxForget $ \i -> k i . fst
  second' (IxForget k) = IxForget $ \i -> k i . snd
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxForget k) = IxForget $ \i -> getConst #. f (Const #. k i)
  {-# INLINE linear #-}

instance Strong IxFunArrow where
  first'  (IxFunArrow k) = IxFunArrow $ \i ~(a, c) -> (k i a, c)
  second' (IxFunArrow k) = IxFunArrow $ \i ~(c, a) -> (c, k i a)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxFunArrow k) = IxFunArrow $ \i -> runIdentity #. f (Identity #. k i)
  {-# INLINE linear #-}

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
  iwander
    :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
    -> p j a b -> p (i -> j) s t

  -- TODO: move this to 'Profunctor' along with ixcontramap.
  conjoined
    :: (forall j. p j a b -> p (     j) s t)
    -> (forall j. p j a b -> p (i -> j) s t)
    -> (forall j. p j a b -> p (i -> j) s t)
  conjoined _ f = f
  {-# INLINE conjoined #-}

  -- TODO: move this to 'Profunctor'
  ixcontramap :: (i -> j) -> p j a b -> p i a b
  default ixcontramap
    :: Coercible (p j a b) (p i a b)
    => (i -> j)
    -> p j a b
    -> p i a b
  ixcontramap _ = coerce
  {-# INLINE ixcontramap #-}

instance Applicative f => Traversing (Star f) where
  wander  f (Star k) = Star $ f k
  iwander f (Star k) = Star $ f (\_ -> k)
  conjoined f _ = coerce f
  {-# INLINE wander #-}
  {-# INLINE iwander #-}
  {-# INLINE conjoined #-}

instance Monoid r => Traversing (Forget r) where
  wander  f (Forget k) = Forget $ getConst #. f (Const #. k)
  iwander f (Forget k) = Forget $ getConst #. f (\_ -> Const #. k)
  conjoined f _ = coerce f
  {-# INLINE wander #-}
  {-# INLINE iwander #-}
  {-# INLINE conjoined #-}

instance Traversing FunArrow where
  wander  f (FunArrow k) = FunArrow $ runIdentity #. f (Identity #. k)
  iwander f (FunArrow k) = FunArrow $ runIdentity #. f (\_ -> Identity #. k)
  conjoined f _ = coerce f
  {-# INLINE wander #-}
  {-# INLINE iwander #-}
  {-# INLINE conjoined #-}

instance Applicative f => Traversing (IxStar f) where
  wander  f (IxStar k) = IxStar $ \i -> f (k i)
  iwander f (IxStar k) = IxStar $ \ij -> f $ \i -> k (ij i)
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

  ixcontramap ij (IxStar k) = IxStar $ \i -> k (ij i)
  {-# INLINE ixcontramap #-}

instance Monoid r => Traversing (IxForget r) where
  wander  f (IxForget k) =
    IxForget $ \i -> getConst #. f (Const #. k i)
  iwander f (IxForget k) =
    IxForget $ \ij -> getConst #. f (\i -> Const #. k (ij i))
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

  ixcontramap ij (IxForget k) = IxForget $ \i -> k (ij i)
  {-# INLINE ixcontramap #-}

instance Traversing IxFunArrow where
  wander  f (IxFunArrow k) =
    IxFunArrow $ \i -> runIdentity #. f (Identity #. k i)
  iwander f (IxFunArrow k) =
    IxFunArrow $ \ij -> runIdentity #. f (\i -> Identity #. k (ij i))
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

  ixcontramap ij (IxFunArrow k) = IxFunArrow $ \i -> k (ij i)
  {-# INLINE ixcontramap #-}

----------------------------------------

class Traversing p => Mapping p where
  roam :: ((a -> b) -> s -> t) -> p i a b -> p i s t
  iroam :: ((i -> a -> b) -> s -> t) -> p j a b -> p (i -> j) s t

instance Mapping FunArrow where
  roam f (FunArrow k) = FunArrow (f k)
  {-# INLINE roam #-}

  iroam f (FunArrow k) = FunArrow (f (const k))
  {-# INLINE iroam #-}

instance Mapping IxFunArrow where
  roam f (IxFunArrow k) = IxFunArrow $ \i -> f (k i)
  {-# INLINE roam #-}

  iroam f (IxFunArrow k) =
    IxFunArrow $ \ij -> f $ \i -> k (ij i)
  {-# INLINE iroam #-}
