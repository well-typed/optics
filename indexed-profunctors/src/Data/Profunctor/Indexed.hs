{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Definitions of concrete profunctors and profunctor classes.
module Data.Profunctor.Indexed
  (
    -- * Profunctor classes
    Profunctor(..)
  , lcoerce
  , rcoerce
  , Strong(..)
  , Costrong(..)
  , Choice(..)
  , Cochoice(..)
  , Visiting(..)
  , Mapping(..)
  , Traversing(..)
  , Traversing1(..)

    -- * Concrete profunctors
  , Star(..)
  , reStar

  , Star1(..)
  , reStar1

  , Forget(..)
  , reForget

  , ForgetM(..)

  , FunArrow(..)
  , reFunArrow

  , IxStar(..)

  , IxForget(..)

  , IxForgetM(..)

  , IxFunArrow(..)

  , StarA(..)
  , runStarA

  , IxStarA(..)
  , runIxStarA

  , Exchange(..)
  , Store(..)
  , Market(..)
  , AffineMarket(..)
  , Tagged(..)
  , Context(..)

   -- * Apply
  , Apply (..)
  , WrappedApplicative (..)

   -- * Utilities
  , (#.)
  , (.#)
  ) where

import Control.Applicative (liftA2)
import Data.Coerce (Coercible, coerce)
import Data.Functor.Const
import Data.Functor.Identity

----------------------------------------
-- Concrete profunctors

-- | Needed for traversals.
newtype Star f i a b = Star { runStar :: a -> f b }

-- | Needed for traversals1.
newtype Star1 f i a b = Star1 { runStar1 :: a -> f b }

-- | Needed for getters and folds.
newtype Forget r i a b = Forget { runForget :: a -> r }

-- | Needed for affine folds.
newtype ForgetM r i a b = ForgetM { runForgetM :: a -> Maybe r }

-- | Needed for setters.
newtype FunArrow i a b = FunArrow { runFunArrow :: a -> b }

-- | Needed for indexed traversals.
newtype IxStar f i a b = IxStar { runIxStar :: i -> a -> f b }

-- | Needed for indexed folds.
newtype IxForget r i a b = IxForget { runIxForget :: i -> a -> r }

-- | Needed for indexed affine folds.
newtype IxForgetM r i a b = IxForgetM { runIxForgetM :: i -> a -> Maybe r }

-- | Needed for indexed setters.
newtype IxFunArrow i a b = IxFunArrow { runIxFunArrow :: i -> a -> b }

----------------------------------------
-- Utils

-- | Needed for conversion of affine traversal back to its VL representation.
data StarA f i a b = StarA (forall r. r -> f r) (a -> f b)

-- | Unwrap 'StarA'.
runStarA :: StarA f i a b -> a -> f b
runStarA (StarA _ k) = k
{-# INLINE runStarA #-}

-- | Needed for conversion of indexed affine traversal back to its VL
-- representation.
data IxStarA f i a b = IxStarA (forall r. r -> f r) (i -> a -> f b)

-- | Unwrap 'StarA'.
runIxStarA :: IxStarA f i a b -> i -> a -> f b
runIxStarA (IxStarA _ k) = k
{-# INLINE runIxStarA #-}

----------------------------------------

-- | Repack 'Star' to change its index type.
reStar :: Star f i a b -> Star f j a b
reStar (Star k) = Star k
{-# INLINE reStar #-}

-- | Repack 'Star1' to change its index type.
reStar1 :: Star1 f i a b -> Star1 f j a b
reStar1 (Star1 k) = Star1 k
{-# INLINE reStar1 #-}

-- | Repack 'Forget' to change its index type.
reForget :: Forget r i a b -> Forget r j a b
reForget (Forget k) = Forget k
{-# INLINE reForget #-}

-- | Repack 'FunArrow' to change its index type.
reFunArrow :: FunArrow i a b -> FunArrow j a b
reFunArrow (FunArrow k) = FunArrow k
{-# INLINE reFunArrow #-}

----------------------------------------
-- Classes and instances

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

  conjoined__
    :: (p i a b -> p i s t)
    -> (p i a b -> p j s t)
    -> (p i a b -> p j s t)
  default conjoined__
    :: Coercible (p i s t) (p j s t)
    => (p i a b -> p i s t)
    -> (p i a b -> p j s t)
    -> (p i a b -> p j s t)
  conjoined__ f _ = coerce . f
  {-# INLINE conjoined__ #-}

  ixcontramap :: (j -> i) -> p i a b -> p j a b
  default ixcontramap
    :: Coercible (p i a b) (p j a b)
    => (j -> i)
    -> p i a b
    -> p j a b
  ixcontramap _ = coerce
  {-# INLINE ixcontramap #-}

-- | 'rcoerce'' with type arguments rearranged for TypeApplications.
rcoerce :: (Coercible a b, Profunctor p) => p i c a -> p i c b
rcoerce = rcoerce'
{-# INLINE rcoerce #-}

-- | 'lcoerce'' with type arguments rearranged for TypeApplications.
lcoerce :: (Coercible a b, Profunctor p) => p i a c -> p i b c
lcoerce = lcoerce'
{-# INLINE lcoerce #-}

instance Functor f => Profunctor (StarA f) where
  dimap f g (StarA point k) = StarA point (fmap g . k . f)
  lmap  f   (StarA point k) = StarA point (k . f)
  rmap    g (StarA point k) = StarA point (fmap g . k)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  rcoerce' = rmap coerce
  {-# INLINE rcoerce' #-}

instance Functor f => Profunctor (Star1 f) where
  dimap f g (Star1 k) = Star1 (fmap g . k . f)
  lmap  f   (Star1 k) = Star1 (k . f)
  rmap    g (Star1 k) = Star1 (fmap g . k)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  rcoerce' = rmap coerce
  {-# INLINE rcoerce' #-}

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

instance Functor f => Profunctor (IxStarA f) where
  dimap f g (IxStarA point k) = IxStarA point (\i -> fmap g . k i . f)
  lmap  f   (IxStarA point k) = IxStarA point (\i -> k i . f)
  rmap    g (IxStarA point k) = IxStarA point (\i -> fmap g . k i)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  rcoerce' = rmap coerce
  {-# INLINE rcoerce' #-}

  conjoined__ _ f = f
  ixcontramap ij (IxStarA point k) = IxStarA point $ \i -> k (ij i)
  {-# INLINE conjoined__ #-}
  {-# INLINE ixcontramap #-}

instance Functor f => Profunctor (IxStar f) where
  dimap f g (IxStar k) = IxStar (\i -> fmap g . k i . f)
  lmap  f   (IxStar k) = IxStar (\i -> k i . f)
  rmap    g (IxStar k) = IxStar (\i -> fmap g . k i)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  rcoerce' = rmap coerce
  {-# INLINE rcoerce' #-}

  conjoined__ _ f = f
  ixcontramap ij (IxStar k) = IxStar $ \i -> k (ij i)
  {-# INLINE conjoined__ #-}
  {-# INLINE ixcontramap #-}

instance Profunctor (IxForget r) where
  dimap f _ (IxForget k) = IxForget (\i -> k i . f)
  lmap  f   (IxForget k) = IxForget (\i -> k i . f)
  rmap   _g (IxForget k) = IxForget k
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  conjoined__ _ f = f
  ixcontramap ij (IxForget k) = IxForget $ \i -> k (ij i)
  {-# INLINE conjoined__ #-}
  {-# INLINE ixcontramap #-}

instance Profunctor (IxForgetM r) where
  dimap f _ (IxForgetM k) = IxForgetM (\i -> k i . f)
  lmap  f   (IxForgetM k) = IxForgetM (\i -> k i . f)
  rmap   _g (IxForgetM k) = IxForgetM k
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  conjoined__ _ f = f
  ixcontramap ij (IxForgetM k) = IxForgetM $ \i -> k (ij i)
  {-# INLINE conjoined__ #-}
  {-# INLINE ixcontramap #-}

instance Profunctor IxFunArrow where
  dimap f g (IxFunArrow k) = IxFunArrow (\i -> g . k i . f)
  lmap  f   (IxFunArrow k) = IxFunArrow (\i -> k i . f)
  rmap    g (IxFunArrow k) = IxFunArrow (\i -> g . k i)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  conjoined__ _ f = f
  ixcontramap ij (IxFunArrow k) = IxFunArrow $ \i -> k (ij i)
  {-# INLINE conjoined__ #-}
  {-# INLINE ixcontramap #-}

----------------------------------------

class Profunctor p => Strong p where
  first'  :: p i a b -> p i (a, c) (b, c)
  second' :: p i a b -> p i (c, a) (c, b)

  -- There are a few places where default implementation is good enough.
  linear
    :: (forall f. Functor f => (a -> f b) -> s -> f t)
    -> p i a b
    -> p i s t
  linear f = dimap
    ((\(Context bt a) -> (a, bt)) . f (Context id))
    (\(b, bt) -> bt b)
    . first'
  {-# INLINE linear #-}

  -- There are a few places where default implementation is good enough.
  ilinear
    :: (forall f. Functor f => (i -> a -> f b) -> s -> f t)
    -> p       j  a b
    -> p (i -> j) s t
  default ilinear
    :: Coercible (p j s t) (p (i -> j) s t)
    => (forall f. Functor f => (i -> a -> f b) -> s -> f t)
    -> p       j  a b
    -> p (i -> j) s t
  ilinear f = coerce . linear (\afb -> f $ \_ -> afb)
  {-# INLINE ilinear #-}

instance Functor f => Strong (StarA f) where
  first'  (StarA point k) = StarA point $ \ ~(a, c) -> (\b' -> (b', c)) <$> k a
  second' (StarA point k) = StarA point $ \ ~(c, a) -> (,) c <$> k a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (StarA point k) = StarA point (f k)
  {-# INLINE linear #-}

instance Functor f => Strong (Star1 f) where
  first'  (Star1 k) = Star1 $ \ ~(a, c) -> (\b' -> (b', c)) <$> k a
  second' (Star1 k) = Star1 $ \ ~(c, a) -> (,) c <$> k a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (Star1 k) = Star1 (f k)
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

instance Functor f => Strong (IxStarA f) where
  first'  (IxStarA point k) = IxStarA point $ \i ~(a, c) -> (\b' -> (b', c)) <$> k i a
  second' (IxStarA point k) = IxStarA point $ \i ~(c, a) -> (,) c <$> k i a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxStarA point k) = IxStarA point $ \i -> f (k i)
  ilinear f (IxStarA point k) = IxStarA point $ \ij -> f $ \i -> k (ij i)
  {-# INLINE linear #-}
  {-# INLINE ilinear #-}

instance Functor f => Strong (IxStar f) where
  first'  (IxStar k) = IxStar $ \i ~(a, c) -> (\b' -> (b', c)) <$> k i a
  second' (IxStar k) = IxStar $ \i ~(c, a) -> (,) c <$> k i a
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxStar k) = IxStar $ \i -> f (k i)
  ilinear f (IxStar k) = IxStar $ \ij -> f $ \i -> k (ij i)
  {-# INLINE linear #-}
  {-# INLINE ilinear #-}

instance Strong (IxForget r) where
  first'  (IxForget k) = IxForget $ \i -> k i . fst
  second' (IxForget k) = IxForget $ \i -> k i . snd
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxForget k) = IxForget $ \i -> getConst #. f (Const #. k i)
  ilinear f (IxForget k) = IxForget $ \ij -> getConst #. f (\i -> Const #. k (ij i))
  {-# INLINE linear #-}
  {-# INLINE ilinear #-}

instance Strong (IxForgetM r) where
  first'  (IxForgetM k) = IxForgetM $ \i -> k i . fst
  second' (IxForgetM k) = IxForgetM $ \i -> k i . snd
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxForgetM k) = IxForgetM $ \i -> getConst #. f (Const #. k i)
  ilinear f (IxForgetM k) = IxForgetM $ \ij -> getConst #. f (\i -> Const #. k (ij i))
  {-# INLINE linear #-}
  {-# INLINE ilinear #-}

instance Strong IxFunArrow where
  first'  (IxFunArrow k) = IxFunArrow $ \i ~(a, c) -> (k i a, c)
  second' (IxFunArrow k) = IxFunArrow $ \i ~(c, a) -> (c, k i a)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

  linear f (IxFunArrow k) = IxFunArrow $ \i ->
    runIdentity #. f (Identity #. k i)
  ilinear f (IxFunArrow k) = IxFunArrow $ \ij ->
    runIdentity #. f (\i -> Identity #. k (ij i))
  {-# INLINE linear #-}
  {-# INLINE ilinear #-}

----------------------------------------

class Profunctor p => Costrong p where
  unfirst  :: p i (a, d) (b, d) -> p i a b
  unsecond :: p i (d, a) (d, b) -> p i a b

----------------------------------------

class Profunctor p => Choice p where
  left'  :: p i a b -> p i (Either a c) (Either b c)
  right' :: p i a b -> p i (Either c a) (Either c b)

instance Functor f => Choice (StarA f) where
  left'  (StarA point k) = StarA point $ either (fmap Left . k) (point . Right)
  right' (StarA point k) = StarA point $ either (point . Left) (fmap Right . k)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

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

instance Functor f => Choice (IxStarA f) where
  left'  (IxStarA point k) =
    IxStarA point $ \i -> either (fmap Left . k i) (point . Right)
  right' (IxStarA point k) =
    IxStarA point $ \i -> either (point . Left) (fmap Right . k i)
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

instance Choice (IxForgetM r) where
  left'  (IxForgetM k) = IxForgetM $ \i -> either (k i) (const Nothing)
  right' (IxForgetM k) = IxForgetM $ \i -> either (const Nothing) (k i)
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

instance Cochoice (IxForgetM r) where
  unleft  (IxForgetM k) = IxForgetM (\i -> k i . Left)
  unright (IxForgetM k) = IxForgetM (\i -> k i . Right)
  {-# INLINE unleft #-}
  {-# INLINE unright #-}

----------------------------------------

class (Choice p, Strong p) => Visiting p where
  visit
    :: forall i s t a b
    . (forall f. Functor f => (forall r. r -> f r) -> (a -> f b) -> s -> f t)
    -> p i a b
    -> p i s t
  visit f =
    let match :: s -> Either a t
        match s = f Right Left s
        update :: s -> b -> t
        update s b = runIdentity $ f Identity (\_ -> Identity b) s
    in dimap (\s -> (match s, s))
             (\(ebt, s) -> either (update s) id ebt)
       . first'
       . left'
  {-# INLINE visit #-}

  ivisit
    :: (forall f. Functor f => (forall r. r -> f r) -> (i -> a -> f b) -> s -> f t)
    -> p       j  a b
    -> p (i -> j) s t
  default ivisit
    :: Coercible (p j s t) (p (i -> j) s t)
    => (forall f. Functor f => (forall r. r -> f r) -> (i -> a -> f b) -> s -> f t)
    -> p       j  a b
    -> p (i -> j) s t
  ivisit f = coerce . visit (\point afb -> f point $ \_ -> afb)
  {-# INLINE ivisit #-}


instance Functor f => Visiting (StarA f) where
  visit  f (StarA point k) = StarA point $ f point k
  ivisit f (StarA point k) = StarA point $ f point (\_ -> k)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Applicative f => Visiting (Star f) where
  visit  f (Star k) = Star $ f pure k
  ivisit f (Star k) = Star $ f pure (\_ -> k)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Monoid r => Visiting (Forget r) where
  visit  f (Forget k) = Forget $ getConst #. f pure (Const #. k)
  ivisit f (Forget k) = Forget $ getConst #. f pure (\_ -> Const #. k)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Visiting (ForgetM r) where
  visit  f (ForgetM k) =
    ForgetM $ getConst #. f (\_ -> Const Nothing) (Const #. k)
  ivisit f (ForgetM k) =
    ForgetM $ getConst #. f (\_ -> Const Nothing) (\_ -> Const #. k)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Visiting FunArrow where
  visit  f (FunArrow k) = FunArrow $ runIdentity #. f pure (Identity #. k)
  ivisit f (FunArrow k) = FunArrow $ runIdentity #. f pure (\_ -> Identity #. k)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Functor f => Visiting (IxStarA f) where
  visit  f (IxStarA point k) = IxStarA point $ \i  -> f point (k i)
  ivisit f (IxStarA point k) = IxStarA point $ \ij -> f point $ \i -> k (ij i)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Applicative f => Visiting (IxStar f) where
  visit  f (IxStar k) = IxStar $ \i  -> f pure (k i)
  ivisit f (IxStar k) = IxStar $ \ij -> f pure $ \i -> k (ij i)
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Monoid r => Visiting (IxForget r) where
  visit  f (IxForget k) =
    IxForget $ \i  -> getConst #. f pure (Const #. k i)
  ivisit f (IxForget k) =
    IxForget $ \ij -> getConst #. f pure (\i -> Const #. k (ij i))
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Visiting (IxForgetM r) where
  visit  f (IxForgetM k) =
    IxForgetM $ \i  -> getConst #. f (\_ -> Const Nothing) (Const #. k i)
  ivisit f (IxForgetM k) =
    IxForgetM $ \ij -> getConst #. f (\_ -> Const Nothing) (\i -> Const #. k (ij i))
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

instance Visiting IxFunArrow where
  visit  f (IxFunArrow k) =
    IxFunArrow $ \i  -> runIdentity #. f pure (Identity #. k i)
  ivisit f (IxFunArrow k) =
    IxFunArrow $ \ij -> runIdentity #. f pure (\i -> Identity #. k (ij i))
  {-# INLINE visit #-}
  {-# INLINE ivisit #-}

----------------------------------------

class Strong p => Traversing1 p where
  wander1
    :: (forall f. Apply f => (a -> f b) -> s -> f t)
    -> p i a b
    -> p i s t
  iwander1
    :: (forall f. Apply f => (i -> a -> f b) -> s -> f t)
    -> p       j  a b
    -> p (i -> j) s t

instance Apply f => Traversing1 (Star1 f) where
  wander1  f (Star1 k) = Star1 $ f k
  iwander1 f (Star1 k) = Star1 $ f (\_ -> k)
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

instance Applicative f => Traversing1 (Star f) where
  wander1  f (Star k) = Star $ unwrapApplicative . f (WrapApplicative . k)
  iwander1 f (Star k) = Star $ unwrapApplicative . f (\_ -> WrapApplicative . k)
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

instance Semigroup r => Traversing1 (Forget r) where
  wander1  f (Forget k) = Forget $ getConst #. f (Const #. k)
  iwander1 f (Forget k) = Forget $ getConst #. f (\_ -> Const #. k)
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

instance Traversing1 FunArrow where
  wander1  f (FunArrow k) = FunArrow $ runIdentity #. f (Identity #. k)
  iwander1 f (FunArrow k) = FunArrow $ runIdentity #. f (\_ -> Identity #. k)
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

instance Applicative f => Traversing1 (IxStar f) where
  wander1  f (IxStar k) = IxStar $ \i -> unwrapApplicative . f (WrapApplicative . k i)
  iwander1 f (IxStar k) = IxStar $ \ij -> unwrapApplicative . f (\i -> WrapApplicative . k (ij i))
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

instance Semigroup r => Traversing1 (IxForget r) where
  wander1  f (IxForget k) =
    IxForget $ \i -> getConst #. f (Const #. k i)
  iwander1 f (IxForget k) =
    IxForget $ \ij -> getConst #. f (\i -> Const #. k (ij i))
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

instance Traversing1 IxFunArrow where
  wander1  f (IxFunArrow k) =
    IxFunArrow $ \i -> runIdentity #. f (Identity #. k i)
  iwander1 f (IxFunArrow k) =
    IxFunArrow $ \ij -> runIdentity #. f (\i -> Identity #. k (ij i))
  {-# INLINE wander1 #-}
  {-# INLINE iwander1 #-}

----------------------------------------

class (Visiting p, Traversing1 p) => Traversing p where
  wander
    :: (forall f. Applicative f => (a -> f b) -> s -> f t)
    -> p i a b
    -> p i s t
  iwander
    :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
    -> p       j  a b
    -> p (i -> j) s t

instance Applicative f => Traversing (Star f) where
  wander  f (Star k) = Star $ f k
  iwander f (Star k) = Star $ f (\_ -> k)
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

instance Monoid r => Traversing (Forget r) where
  wander  f (Forget k) = Forget $ getConst #. f (Const #. k)
  iwander f (Forget k) = Forget $ getConst #. f (\_ -> Const #. k)
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

instance Traversing FunArrow where
  wander  f (FunArrow k) = FunArrow $ runIdentity #. f (Identity #. k)
  iwander f (FunArrow k) = FunArrow $ runIdentity #. f (\_ -> Identity #. k)
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

instance Applicative f => Traversing (IxStar f) where
  wander  f (IxStar k) = IxStar $ \i -> f (k i)
  iwander f (IxStar k) = IxStar $ \ij -> f $ \i -> k (ij i)
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

instance Monoid r => Traversing (IxForget r) where
  wander  f (IxForget k) =
    IxForget $ \i -> getConst #. f (Const #. k i)
  iwander f (IxForget k) =
    IxForget $ \ij -> getConst #. f (\i -> Const #. k (ij i))
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

instance Traversing IxFunArrow where
  wander  f (IxFunArrow k) =
    IxFunArrow $ \i -> runIdentity #. f (Identity #. k i)
  iwander f (IxFunArrow k) =
    IxFunArrow $ \ij -> runIdentity #. f (\i -> Identity #. k (ij i))
  {-# INLINE wander #-}
  {-# INLINE iwander #-}

----------------------------------------

class Traversing p => Mapping p where
  roam
    :: ((a -> b) -> s -> t)
    -> p i a b
    -> p i s t
  iroam
    :: ((i -> a -> b) -> s -> t)
    -> p       j  a b
    -> p (i -> j) s t

instance Mapping FunArrow where
  roam  f (FunArrow k) = FunArrow $ f k
  iroam f (FunArrow k) = FunArrow $ f (const k)
  {-# INLINE roam #-}
  {-# INLINE iroam #-}

instance Mapping IxFunArrow where
  roam  f (IxFunArrow k) = IxFunArrow $ \i -> f (k i)
  iroam f (IxFunArrow k) = IxFunArrow $ \ij -> f $ \i -> k (ij i)
  {-# INLINE roam #-}
  {-# INLINE iroam #-}


  -- | Type to represent the components of an isomorphism.
data Exchange a b i s t =
  Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap ss tt (Exchange sa bt) = Exchange (sa . ss) (tt . bt)
  lmap  ss    (Exchange sa bt) = Exchange (sa . ss) bt
  rmap     tt (Exchange sa bt) = Exchange sa        (tt . bt)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

-- | Type to represent the components of a lens.
data Store a b i s t = Store (s -> a) (s -> b -> t)

instance Profunctor (Store a b) where
  dimap f g (Store get set) = Store (get . f) (\s -> g . set (f s))
  lmap  f   (Store get set) = Store (get . f) (\s -> set (f s))
  rmap    g (Store get set) = Store get       (\s -> g . set s)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Strong (Store a b) where
  first' (Store get set) = Store (get . fst) (\(s, c) b -> (set s b, c))
  second' (Store get set) = Store (get . snd) (\(c, s) b -> (c, set s b))
  {-# INLINE first' #-}
  {-# INLINE second' #-}

-- | Type to represent the components of a prism.
data Market a b i s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b i s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  lmap  f   (Market bt seta) = Market bt (seta . f)
  rmap    g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE left' #-}
  {-# INLINE right' #-}

-- | Type to represent the components of an affine traversal.
data AffineMarket a b i s t = AffineMarket (s -> b -> t) (s -> Either t a)

instance Profunctor (AffineMarket a b) where
  dimap f g (AffineMarket sbt seta) = AffineMarket
    (\s b -> g (sbt (f s) b))
    (either (Left . g) Right . seta . f)
  lmap f (AffineMarket sbt seta) = AffineMarket
    (\s b -> sbt (f s) b)
    (seta . f)
  rmap g (AffineMarket sbt seta) = AffineMarket
    (\s b -> g (sbt s b))
    (either (Left . g) Right . seta)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice (AffineMarket a b) where
  left' (AffineMarket sbt seta) = AffineMarket
    (\e b -> bimap (flip sbt b) id e)
    (\sc -> case sc of
      Left s -> bimap Left id (seta s)
      Right c -> Left (Right c))
  right' (AffineMarket sbt seta) = AffineMarket
    (\e b -> bimap id (flip sbt b) e)
    (\sc -> case sc of
      Left c -> Left (Left c)
      Right s -> bimap Right id (seta s))
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Strong (AffineMarket a b) where
  first' (AffineMarket sbt seta) = AffineMarket
    (\(a, c) b -> (sbt a b, c))
    (\(a, c) -> bimap (,c) id (seta a))
  second' (AffineMarket sbt seta) = AffineMarket
    (\(c, a) b -> (c, sbt a b))
    (\(c, a) -> bimap (c,) id (seta a))
  {-# INLINE first' #-}
  {-# INLINE second' #-}

bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
bimap f g = either (Left . f) (Right . g)

instance Visiting (AffineMarket a b)


-- | Tag a value with not one but two phantom type parameters (so that 'Tagged'
-- can be used as an indexed profunctor).
newtype Tagged i a b = Tagged { unTagged :: b }

instance Functor (Tagged i a) where
  fmap f = Tagged #. f .# unTagged
  {-# INLINE fmap #-}

instance Profunctor Tagged where
  dimap _f g = Tagged #. g .# unTagged
  lmap  _f   = coerce
  rmap     g = Tagged #. g .# unTagged
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice Tagged where
  left'  = Tagged #. Left  .# unTagged
  right' = Tagged #. Right .# unTagged
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Costrong Tagged where
  unfirst (Tagged bd) = Tagged (fst bd)
  unsecond (Tagged db) = Tagged (snd db)
  {-# INLINE unfirst #-}
  {-# INLINE unsecond #-}


data Context a b t = Context (b -> t) a
  deriving Functor


----------------------------------------

-- | This is equivalent to an 'Applicative' without 'pure'.
--
-- An own version to avoid dependency on @semigroupoids@.
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b
  (<.>) = liftF2 ($)
  {-# INLINE (<.>) #-}

  (.>) :: f a -> f b -> f b
  (.>) = liftF2 (\_ b -> b)  

  (<.) :: f a -> f b -> f a
  (<.) = liftF2 (\a _ -> a)  

  liftF2 :: (a -> b -> c) -> f a -> f b -> f c
  liftF2 f x y = f <$> x <.> y
  {-# INLINE liftF2 #-}

  {-# MINIMAL (<.>) | liftF2 #-}

infixl 4 <.>

instance Apply Identity where
  (<.>) = (<*>)
  liftF2 = liftA2

instance Semigroup b => Apply (Const b) where
  Const x <.> Const y = Const (x <> y)

-- | Wrap an 'Applicative' to be used as a member of 'Apply'
newtype WrappedApplicative f a = WrapApplicative { unwrapApplicative :: f a }

instance Functor f => Functor (WrappedApplicative f) where
  fmap f (WrapApplicative a) = WrapApplicative (f <$> a)

instance Applicative f => Apply (WrappedApplicative f) where
  WrapApplicative f <.> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <.  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  .> WrapApplicative b = WrapApplicative (a  *> b)

instance Applicative f => Applicative (WrappedApplicative f) where
  pure = WrapApplicative . pure
  WrapApplicative f <*> WrapApplicative a = WrapApplicative (f <*> a)
  WrapApplicative a <*  WrapApplicative b = WrapApplicative (a <*  b)
  WrapApplicative a  *> WrapApplicative b = WrapApplicative (a  *> b)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

----------------------------------------

-- | Composition operator where the first argument must be an identity
-- function up to representational equivalence (e.g. a newtype wrapper
-- or unwrapper), and will be ignored at runtime.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
infixl 8 .#
{-# INLINE (#.) #-}

-- | Composition operator where the second argument must be an
-- identity function up to representational equivalence (e.g. a
-- newtype wrapper or unwrapper), and will be ignored at runtime.
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _g = coerce f
infixr 9 #.
{-# INLINE (.#) #-}
