{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Optics.Zoom
  ( Magnify(..)
  , Zoom(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS.Lazy as L
import Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.State.Lazy as L
import Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer.Lazy as L
import Control.Monad.Trans.Writer.Strict as S
import Data.Coerce
import Data.Monoid

import Optics.Core
import Optics.Internal.Utils
import Optics.Extra.Internal.Zoom

-- $setup
-- >>> import Optics.Core
-- >>> import Optics.Operators.State
-- >>> import Control.Monad.State
-- >>> import Data.Map as Map
-- >>> import Debug.SimpleReflect.Expr as Expr
-- >>> import Debug.SimpleReflect.Vars as Vars
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let h :: Expr -> Expr -> Expr; h = Debug.SimpleReflect.Vars.h

-- Chosen so that they have lower fixity than ('%=').
infixr 2 `zoom`, `zoomMaybe`, `zoomMany`
infixr 2 `magnify`, `magnifyMaybe`, `magnifyMany`

------------------------------------------------------------------------------
-- Zoom
------------------------------------------------------------------------------

-- | This class allows us to 'zoom' in, changing the 'State' supplied by many
-- different monad transformers, potentially quite deep in a monad transformer
-- stack.
--
-- Its functions can be used to run a monadic action in a larger 'State' than it
-- was defined in, using a 'Lens'', an 'AffineTraversal'' or a 'Traversal''.
--
-- This is commonly used to lift actions in a simpler 'State' 'Monad' into a
-- 'State' 'Monad' with a larger 'State' type.
--
-- When used with a 'Traversal'' over multiple values, the actions for each
-- target are executed sequentially and the results are aggregated.
--
-- This can be used to edit pretty much any 'Monad' transformer stack with a
-- 'State' in it!
--
-- >>> flip evalState (a,b) $ zoom _1 $ use equality
-- a
--
-- >>> flip execState (a,b) $ zoom _1 $ equality .= c
-- (c,b)
--
-- >>> flip execState [(a,b),(c,d)] $ zoomMany traversed $ _2 %= f
-- [(a,f b),(c,f d)]
--
-- >>> flip unState [(a,b),(c,d)] $ zoomMany traversed $ _2 <%= f
-- (f b <> f d <> mempty,[(a,f b),(c,f d)])
--
-- >>> flip evalState (a,b) $ zoomMany each (use equality)
-- a <> b
--
class
  (MonadState s m, MonadState t n
  ) => Zoom m n s t | m -> s, n -> t, m t -> n, n s -> m where
  zoom
    :: Is k A_Lens
    => Optic' k is t s
    -> m c
    -> n c

  zoomMaybe
    :: Is k An_AffineTraversal
    => Optic' k is t s
    -> m c
    -> n (Maybe c)

  zoomMany
    :: (Is k A_Traversal, Monoid c)
    => Optic' k is t s
    -> m c
    -> n c

instance Monad m => Zoom (S.StateT s m) (S.StateT t m) s t where
  zoom      o = \(S.StateT m) -> S.StateT $ stateZoom      o m
  zoomMaybe o = \(S.StateT m) -> S.StateT $ stateZoomMaybe o m
  zoomMany  o = \(S.StateT m) -> S.StateT $ stateZoomMany  o m
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance Monad m => Zoom (L.StateT s m) (L.StateT t m) s t where
  zoom      o = \(L.StateT m) -> L.StateT $ stateZoom      o m
  zoomMaybe o = \(L.StateT m) -> L.StateT $ stateZoomMaybe o m
  zoomMany  o = \(L.StateT m) -> L.StateT $ stateZoomMany  o m
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance Zoom m n s t => Zoom (ReaderT e m) (ReaderT e n) s t where
  zoom      o = \(ReaderT m) -> ReaderT (zoom      o . m)
  zoomMaybe o = \(ReaderT m) -> ReaderT (zoomMaybe o . m)
  zoomMany  o = \(ReaderT m) -> ReaderT (zoomMany  o . m)
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance Zoom m n s t => Zoom (IdentityT m) (IdentityT n) s t where
  zoom      o = \(IdentityT m) -> IdentityT (zoom      o m)
  zoomMaybe o = \(IdentityT m) -> IdentityT (zoomMaybe o m)
  zoomMany  o = \(IdentityT m) -> IdentityT (zoomMany  o m)
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance (Monoid w, Monad m) => Zoom (S.RWST r w s m) (S.RWST r w t m) s t where
  zoom      o = \(S.RWST m) -> S.RWST $ rwsZoom      o m
  zoomMaybe o = \(S.RWST m) -> S.RWST $ rwsZoomMaybe o m
  zoomMany  o = \(S.RWST m) -> S.RWST $ rwsZoomMany  o m
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance (Monoid w, Monad m) => Zoom (L.RWST r w s m) (L.RWST r w t m) s t where
  zoom      o = \(L.RWST m) -> L.RWST $ rwsZoom      o m
  zoomMaybe o = \(L.RWST m) -> L.RWST $ rwsZoomMaybe o m
  zoomMany  o = \(L.RWST m) -> L.RWST $ rwsZoomMany  o m
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance (Monoid w, Zoom m n s t) => Zoom (S.WriterT w m) (S.WriterT w n) s t where
  zoom      o = S.WriterT #.                 zoom      o .# S.runWriterT
  zoomMaybe o = S.WriterT #. fmap shuffleW . zoomMaybe o .# S.runWriterT
  zoomMany  o = S.WriterT #.                 zoomMany  o .# S.runWriterT
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance (Monoid w, Zoom m n s t) => Zoom (L.WriterT w m) (L.WriterT w n) s t where
  zoom      o = L.WriterT #.                 zoom      o .# L.runWriterT
  zoomMaybe o = L.WriterT #. fmap shuffleW . zoomMaybe o .# L.runWriterT
  zoomMany  o = L.WriterT #.                 zoomMany  o .# L.runWriterT
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance Zoom m n s t => Zoom (ListT m) (ListT n) s t where
  zoom      o = ListT #.                  zoom      o .# runListT
  zoomMaybe o = ListT #. fmap sequenceA . zoomMaybe o .# runListT
  zoomMany  o = ListT #.                  zoomMany  o .# runListT
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance Zoom m n s t => Zoom (MaybeT m) (MaybeT n) s t where
  zoom o =
    MaybeT #. zoom o .# runMaybeT
  zoomMaybe o =
    MaybeT #. fmap (getMay . shuffleMay) . zoomMaybe o . fmap May .# runMaybeT
  zoomMany o =
    MaybeT #. fmap getMay . zoomMany o . fmap May .# runMaybeT
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance (Error e, Zoom m n s t) => Zoom (ErrorT e m) (ErrorT e n) s t where
  zoom o =
    ErrorT #. zoom o .# runErrorT
  zoomMaybe o =
    ErrorT #. fmap (getErr . shuffleErr) . zoomMaybe o . fmap Err .# runErrorT
  zoomMany o =
    ErrorT #. fmap getErr . zoomMany o . fmap Err .# runErrorT
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

instance Zoom m n s t => Zoom (ExceptT e m) (ExceptT e n) s t where
  zoom o =
    ExceptT #. zoom o .# runExceptT
  zoomMaybe o =
    ExceptT #. fmap (getErr . shuffleErr) . zoomMaybe o . fmap Err .# runExceptT
  zoomMany o =
    ExceptT #. fmap getErr . zoomMany o . fmap Err .# runExceptT
  {-# INLINE zoom #-}
  {-# INLINE zoomMaybe #-}
  {-# INLINE zoomMany #-}

-- TODO: instance Zoom m m a a => Zoom (ContT r m) (ContT r m) a a where

----------------------------------------
-- Zoom helpers

stateZoom
  :: (Is k A_Lens, Monad m)
  => Optic' k is t s
  -> (s -> m (c, s))
  -> (t -> m (c, t))
stateZoom o m = unfocusing #. toLensVL o (Focusing #. m)

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

stateZoomMany
  :: (Is k A_Traversal, Monad m, Monoid c)
  => Optic' k is t s
  -> (s -> m (c, s))
  -> (t -> m (c, t))
stateZoomMany o m = unfocusing #. traverseOf o (Focusing #. m)

rwsZoom
  :: (Is k A_Lens, Monad m)
  => Optic' k is t s
  -> (r -> s -> m (c, s, w))
  -> (r -> t -> m (c, t, w))
rwsZoom o m r = unfocusingWith #. toLensVL o (FocusingWith #. m r)

rwsZoomMaybe
  :: (Is k An_AffineTraversal, Monad m, Monoid w)
  => Optic' k is t s
  -> (r -> s -> m (c, s, w))
  -> (r -> t -> m (Maybe c, t, w))
rwsZoomMaybe o m r =
     fmap (coerce :: (First c, t, w) -> (Maybe c, t, w))
  .  unfocusingWith
  #. traverseOf (toAffineTraversal o)
                (FocusingWith #. over (mapped % _1) (First #. Just) . m r)

rwsZoomMany
  :: (Is k A_Traversal, Monad m, Monoid w, Monoid c)
  => Optic' k is t s
  -> (r -> s -> m (c, s, w))
  -> (r -> t -> m (c, t, w))
rwsZoomMany o m r = unfocusingWith #. traverseOf o (FocusingWith #. m r)

shuffleW :: Monoid w => Maybe (c, w) -> (Maybe c, w)
shuffleW = maybe (Nothing, mempty) (over _1 Just)

shuffleMay :: Maybe (May c) -> May (Maybe c)
shuffleMay = \case
  Nothing      -> May (Just Nothing)
  Just (May c) -> May (Just <$> c)

shuffleErr :: Maybe (Err e c) -> Err e (Maybe c)
shuffleErr = \case
  Nothing       -> Err (Right Nothing)
  Just (Err ec) -> Err (Just <$> ec)

------------------------------------------------------------------------------
-- Magnify
------------------------------------------------------------------------------

-- | This class allows us to 'magnify' part of the environment, changing the
-- environment supplied by many different 'Monad' transformers. Unlike 'zoom'
-- this can change the environment of a deeply nested 'Monad' transformer.
--
-- Its functions can be used to run a monadic action in a larger environment
-- than it was defined in, using a 'Getter'', an 'AffineFold'' or a 'Fold''.
--
-- They act like 'Control.Monad.Reader.Class.local', but can in many cases
-- change the type of the environment as well.
--
-- They're commonly used to lift actions in a simpler 'Reader' 'Monad' into a
-- 'Monad' with a larger environment type.
--
-- When used with a 'Fold'' over multiple values, the actions for each target
-- are executed sequentially and the results are aggregated.
--
-- They can be used to edit pretty much any 'Monad' transformer stack with an
-- environment in it:
--
-- >>> (1,2) & magnify _2 (+1)
-- 3
--
-- >>> flip runReader (1,2) $ magnify _1 ask
-- 1
--
-- >>> flip runReader (1,2,[10..20]) $ magnifyMaybe (_3 % _tail) ask
-- Just [11,12,13,14,15,16,17,18,19,20]
--
class
  (MonadReader b m, MonadReader a n
  ) => Magnify m n b a | m -> b, n -> a, m a -> n, n b -> m where
  magnify
    :: Is k A_Getter
    => Optic' k is a b
    -> m c
    -> n c

  magnifyMaybe
    :: Is k An_AffineFold
    => Optic' k is a b
    -> m c
    -> n (Maybe c)

  magnifyMany
    :: (Is k A_Fold, Monoid c)
    => Optic' k is a b
    -> m c
    -> n c

-- | @
-- 'magnify'      = 'views'
-- 'magnifyMaybe' = 'previews'
-- 'magnifyMany'  = 'foldMapOf'
-- @
instance Magnify ((->) b) ((->) a) b a where
  magnify      = views
  magnifyMaybe = previews
  magnifyMany  = foldMapOf
  {-# INLINE magnify #-}
  {-# INLINE magnifyMaybe #-}
  {-# INLINE magnifyMany #-}

instance Monad m => Magnify (ReaderT b m) (ReaderT a m) b a where
  magnify o = \(ReaderT m) ->
    ReaderT $ \r -> getEffect (views o (Effect #. m) r)
  magnifyMaybe o = \(ReaderT m) ->
    ReaderT $ \r -> traverse getEffect (previews o (Effect #. m) r)
  magnifyMany o = \(ReaderT m) ->
    ReaderT $ \r -> getEffect (foldMapOf o (Effect #. m) r)
  {-# INLINE magnify #-}
  {-# INLINE magnifyMaybe #-}
  {-# INLINE magnifyMany #-}

instance (Monad m, Monoid w) => Magnify (S.RWST b w s m) (S.RWST a w s m) b a where
  magnify      o = \(S.RWST m) -> S.RWST $ rwsMagnify      o m
  magnifyMaybe o = \(S.RWST m) -> S.RWST $ rwsMagnifyMaybe o m
  magnifyMany  o = \(S.RWST m) -> S.RWST $ rwsMagnifyMany  o m
  {-# INLINE magnify #-}
  {-# INLINE magnifyMaybe #-}
  {-# INLINE magnifyMany #-}

instance (Monad m, Monoid w) => Magnify (L.RWST b w s m) (L.RWST a w s m) b a where
  magnify      o = \(L.RWST m) -> L.RWST $ rwsMagnify      o m
  magnifyMaybe o = \(L.RWST m) -> L.RWST $ rwsMagnifyMaybe o m
  magnifyMany  o = \(L.RWST m) -> L.RWST $ rwsMagnifyMany  o m
  {-# INLINE magnify #-}
  {-# INLINE magnifyMaybe #-}
  {-# INLINE magnifyMany #-}

instance Magnify m n b a => Magnify (IdentityT m) (IdentityT n) b a where
  magnify      o = \(IdentityT m) -> IdentityT (magnify      o m)
  magnifyMaybe o = \(IdentityT m) -> IdentityT (magnifyMaybe o m)
  magnifyMany  o = \(IdentityT m) -> IdentityT (magnifyMany  o m)
  {-# INLINE magnify #-}
  {-# INLINE magnifyMaybe #-}
  {-# INLINE magnifyMany #-}

----------------------------------------
-- Magnify helpers

rwsMagnify
  :: Is k A_Getter
  => Optic' k is a b
  -> (b -> s -> f (c, s, w))
  -> (a -> s -> f (c, s, w))
rwsMagnify o m = getEffectRWS #. views o (EffectRWS #. m)

rwsMagnifyMaybe
  :: (Is k An_AffineFold, Applicative m, Monoid w)
  => Optic' k is a b
  -> (b -> s -> m (c, s, w))
  -> (a -> s -> m (Maybe c, s, w))
rwsMagnifyMaybe o m r s = maybe
  (pure (Nothing, s, mempty))
  (\e -> over (mapped % _1) Just $ getEffectRWS e s)
  (previews o (EffectRWS #. m) r)

rwsMagnifyMany
  :: (Is k A_Fold, Monad m, Monoid w, Monoid c)
  => Optic' k is a b
  -> (b -> s -> m (c, s, w))
  -> (a -> s -> m (c, s, w))
rwsMagnifyMany o m = getEffectRWS #. foldMapOf o (EffectRWS #. m)
