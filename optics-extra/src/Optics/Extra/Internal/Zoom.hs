module Optics.Extra.Internal.Zoom
  (
  -- * Zoom
    Focusing(..)
  , stateZoom
  , stateZoomMaybe
  , stateZoomMany
  , FocusingWith(..)
  , rwsZoom
  , rwsZoomMaybe
  , rwsZoomMany
  , May(..)
  , shuffleMay
  , Err(..)
  , shuffleErr
  -- * Magnify
  , Effect(..)
  , EffectRWS(..)
  , rwsMagnify
  , rwsMagnifyMaybe
  , rwsMagnifyMany
  -- * Misc
  , shuffleS
  , shuffleW
  ) where

import Data.Coerce
import Data.Monoid
import qualified Data.Semigroup as SG

import Optics.Core
import Optics.Internal.Utils

-- | Used by 'Optics.Zoom.Zoom' to 'Optics.Zoom.zoom' into
-- 'Control.Monad.State.StateT'.
newtype Focusing m c s = Focusing { unfocusing :: m (c, s) }

instance Monad m => Functor (Focusing m c) where
  fmap f (Focusing m) = Focusing $ do
     (c, s) <- m
     pure (c, f s)
  {-# INLINE fmap #-}

instance (Monad m, Monoid s) => Applicative (Focusing m s) where
  pure s = Focusing $ pure (mempty, s)
  Focusing mf <*> Focusing ms = Focusing $ do
    (c, f) <- mf
    (c', s) <- ms
    pure (c `mappend` c', f s)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

stateZoom
  :: (Is k A_Lens, Monad m)
  => Optic' k is t s
  -> (s -> m (c, s))
  -> (t -> m (c, t))
stateZoom o m = unfocusing #. toLensVL o (Focusing #. m)
{-# INLINE stateZoom #-}

stateZoomMaybe
  :: (Is k An_AffineTraversal, Monad m)
  => Optic' k is t s
  -> (s -> m (c, s))
  -> (t -> m (Maybe c, t))
stateZoomMaybe o m =
     fmap (coerce :: (First c, t) -> (Maybe c, t))
  .  unfocusing
  #. traverseOf (toAffineTraversal o)
                (Focusing #. over (mapped % _1) (First #. Just) . m)
{-# INLINE stateZoomMaybe #-}

stateZoomMany
  :: (Is k A_Traversal, Monad m, Monoid c)
  => Optic' k is t s
  -> (s -> m (c, s))
  -> (t -> m (c, t))
stateZoomMany o m = unfocusing #. traverseOf o (Focusing #. m)
{-# INLINE stateZoomMany #-}

----------------------------------------

-- | Used by 'Optics.Zoom.Zoom' to 'Optics.Zoom.zoom' into
-- 'Control.Monad.RWS.RWST'.
newtype FocusingWith w m c s = FocusingWith { unfocusingWith :: m (c, s, w) }

instance Monad m => Functor (FocusingWith w m s) where
  fmap f (FocusingWith m) = FocusingWith $ do
     (c, s, w) <- m
     pure (c, f s, w)
  {-# INLINE fmap #-}

instance (Monad m, Monoid s, Monoid w) => Applicative (FocusingWith w m s) where
  pure s = FocusingWith $ pure (mempty, s, mempty)
  FocusingWith mf <*> FocusingWith ms = FocusingWith $ do
    (c, f, w) <- mf
    (c', s, w') <- ms
    pure (c `mappend` c', f s, w `mappend` w')
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

rwsZoom
  :: (Is k A_Lens, Monad m)
  => Optic' k is t s
  -> (r -> s -> m (c, s, w))
  -> (r -> t -> m (c, t, w))
rwsZoom o m = \r -> unfocusingWith #. toLensVL o (FocusingWith #. m r)
{-# INLINE rwsZoom #-}

rwsZoomMaybe
  :: (Is k An_AffineTraversal, Monad m, Monoid w)
  => Optic' k is t s
  -> (r -> s -> m (c, s, w))
  -> (r -> t -> m (Maybe c, t, w))
rwsZoomMaybe o m = \r ->
     fmap (coerce :: (First c, t, w) -> (Maybe c, t, w))
  .  unfocusingWith
  #. traverseOf (toAffineTraversal o)
                (FocusingWith #. over (mapped % _1) (First #. Just) . m r)
{-# INLINE rwsZoomMaybe #-}

rwsZoomMany
  :: (Is k A_Traversal, Monad m, Monoid w, Monoid c)
  => Optic' k is t s
  -> (r -> s -> m (c, s, w))
  -> (r -> t -> m (c, t, w))
rwsZoomMany o m = \r -> unfocusingWith #. traverseOf o (FocusingWith #. m r)
{-# INLINE rwsZoomMany #-}

----------------------------------------

-- | Make a 'Monoid' out of 'Maybe' for error handling.
newtype May a = May { getMay :: Maybe a }

instance SG.Semigroup a => SG.Semigroup (May a) where
  May (Just a) <> May (Just b) = May $ Just (a SG.<> b)
  _            <> _            = May Nothing
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (May a) where
  mempty = May $ Just mempty
  May (Just a) `mappend` May (Just b) = May $ Just (a `mappend` b)
  _            `mappend` _            = May Nothing
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

shuffleMay :: Maybe (May c) -> May (Maybe c)
shuffleMay = \case
  Nothing      -> May (Just Nothing)
  Just (May c) -> May (Just <$> c)
{-# INLINE shuffleMay #-}

----------------------------------------

-- | Make a 'Monoid' out of 'Either' for error handling.
newtype Err e a = Err { getErr :: Either e a }

instance SG.Semigroup a => SG.Semigroup (Err e a) where
  Err (Right a) <> Err (Right b) = Err $ Right (a SG.<> b)
  Err (Left e)  <> _             = Err $ Left e
  _             <> Err (Left e)  = Err $ Left e
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Err e a) where
  mempty = Err $ Right mempty
  Err (Right a) `mappend` Err (Right b) = Err $ Right (a `mappend` b)
  Err (Left e)  `mappend` _             = Err $ Left e
  _             `mappend` Err (Left e)  = Err $ Left e
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

shuffleErr :: Maybe (Err e c) -> Err e (Maybe c)
shuffleErr = \case
  Nothing       -> Err (Right Nothing)
  Just (Err ec) -> Err (Just <$> ec)
{-# INLINE shuffleErr #-}

----------------------------------------

-- | Wrap a monadic effect.
newtype Effect m r = Effect { getEffect :: m r }

instance (Monad m, SG.Semigroup r) => SG.Semigroup (Effect m r) where
  Effect ma <> Effect mb = Effect $ (SG.<>) <$> ma <*> mb
  {-# INLINE (<>) #-}

instance (Monad m, Monoid r) => Monoid (Effect m r) where
  mempty = Effect $ pure mempty
  Effect ma `mappend` Effect mb = Effect $ mappend <$> ma <*> mb
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

----------------------------------------

-- | Wrap a monadic effect. Used when magnifying 'Control.Monad.RWS.RWST'.
newtype EffectRWS w s m c = EffectRWS { getEffectRWS :: s -> m (c, s, w) }

instance
  (SG.Semigroup c, SG.Semigroup w, Monad m
  ) => SG.Semigroup (EffectRWS w s m c) where
  EffectRWS ma <> EffectRWS mb = EffectRWS $ \s -> do
    (c, s', w)    <- ma s
    (c', s'', w') <- mb s'
    pure (c SG.<> c', s'', w SG.<> w')
  {-# INLINE (<>) #-}

instance (Monoid c, Monoid w, Monad m) => Monoid (EffectRWS w s m c) where
  mempty  = EffectRWS $ \s -> pure (mempty, s, mempty)
  EffectRWS ma `mappend` EffectRWS mb = EffectRWS $ \s -> do
    (c, s', w)    <- ma s
    (c', s'', w') <- mb s'
    pure (c `mappend` c', s'', w `mappend` w')
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

rwsMagnify
  :: Is k A_Getter
  => Optic' k is a b
  -> (b -> s -> f (c, s, w))
  -> (a -> s -> f (c, s, w))
rwsMagnify o m = getEffectRWS #. views o (EffectRWS #. m)
{-# INLINE rwsMagnify #-}

rwsMagnifyMaybe
  :: (Is k An_AffineFold, Applicative m, Monoid w)
  => Optic' k is a b
  -> (b -> s -> m (c, s, w))
  -> (a -> s -> m (Maybe c, s, w))
rwsMagnifyMaybe o m = \r s -> maybe
  (pure (Nothing, s, mempty))
  (\e -> over (mapped % _1) Just $ getEffectRWS e s)
  (previews o (EffectRWS #. m) r)
{-# INLINE rwsMagnifyMaybe #-}

rwsMagnifyMany
  :: (Is k A_Fold, Monad m, Monoid w, Monoid c)
  => Optic' k is a b
  -> (b -> s -> m (c, s, w))
  -> (a -> s -> m (c, s, w))
rwsMagnifyMany o m = getEffectRWS #. foldMapOf o (EffectRWS #. m)
{-# INLINE rwsMagnifyMany #-}

----------------------------------------
-- Misc

shuffleS :: s -> Maybe (c, s) -> (Maybe c, s)
shuffleS s = maybe (Nothing, s) (over _1 Just)
{-# INLINE shuffleS #-}

shuffleW :: Monoid w => Maybe (c, w) -> (Maybe c, w)
shuffleW = maybe (Nothing, mempty) (over _1 Just)
{-# INLINE shuffleW #-}
