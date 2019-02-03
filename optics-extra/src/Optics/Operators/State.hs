module Optics.Operators.State (
  -- * State modifying optics
  (.=), (?=), (%=),
  -- * State modifying optics with passthrough
  (%%=),
  -- * Returning new value
  (<.=), (<?=), (<%=),
  -- * Returning old value
  (<<.=), (<<?=), (<<%=),
  -- * Passthrough
  PermeableOptic (..),
  ) where

import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

import Optics.Core
import Optics.Operators
import Optics.Passthrough
import Optics.View

infix 4 .=, ?=, %=

(.=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m ()
o .= x = o %= const x
{-# INLINE (.=) #-}

(?=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m ()
o ?= x = o %= const (Just x)
{-# INLINE (?=) #-}

(%=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m ()
o %= f = State.modify (o %~ f)
{-# INLINE (%=) #-}

-------------------------------------------------------------------------------
-- Extra stuff
-------------------------------------------------------------------------------

infix 4 %%=
(%%=)
  :: (PermeableOptic k r, MonadState s m)
  => Optic k is s s a b
  -> (a -> (r, b))
  -> m (ViewResult k r)
o %%= f = State.state (passthrough o f)
{-# INLINE (%%=) #-}

-------------------------------------------------------------------------------
-- Returning new value
-------------------------------------------------------------------------------

infix 4 <.=, <?=, <%=

(<%=)
  :: (PermeableOptic k b, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m (ViewResult k b)
o <%= f = o %%= \a -> let b = f a in (b, b)
{-# INLINE (<%=) #-}

(<?=)
  :: (PermeableOptic k (Maybe b), MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m (ViewResult k (Maybe b))
o <?= b = o <.= Just b
{-# INLINE (<?=) #-}

(<.=)
  :: (PermeableOptic k b, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m (ViewResult k b)
o <.= b = o <%= const b
{-# INLINE (<.=) #-}

-------------------------------------------------------------------------------
-- Returning old value
-------------------------------------------------------------------------------

infix 4 <<.=, <<?=, <<%=

(<<%=)
  :: (PermeableOptic k a, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m (ViewResult k a)
o <<%= f = o %%= \a -> (a, f a)
{-# INLINE (<<%=) #-}

(<<?=)
  :: (PermeableOptic k (Maybe a), MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m (ViewResult k (Maybe a))
o <<?= b = o <<.= Just b
{-# INLINE (<<?=) #-}

(<<.=)
  :: (PermeableOptic k a, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m (ViewResult k a)
o <<.= b = o <<%= const b
{-# INLINE (<<.=) #-}
