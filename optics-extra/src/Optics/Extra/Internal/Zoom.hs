module Optics.Extra.Internal.Zoom
  (
  -- * Zoom
    Focusing(..)
  , FocusingWith(..)
  , May(..)
  , Err(..)
  -- * Magnify
  , Effect(..)
  , EffectRWS(..)
  ) where

import qualified Data.Semigroup as SG

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
