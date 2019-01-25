{-# LANGUAGE FlexibleContexts #-}
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

infix 4 .=, ?=, %=

(.=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m ()
o .= x = o %= const x

(?=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m ()
o ?= x = o %= const (Just x)

(%=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m ()
o %= f = State.modify (o %~ f)

-------------------------------------------------------------------------------
-- Extra stuff
-------------------------------------------------------------------------------

infix 4 %%=
(%%=)
  :: (PermeableOptic k is r, MonadState s m)
  => Optic k is s s a b
  -> (a -> (r, b))
  -> m (ViewResult k is r)
o %%= f = State.state (passthrough o f)

-------------------------------------------------------------------------------
-- Returning new value
-------------------------------------------------------------------------------

infix 4 <.=, <?=, <%=

(<%=)
  :: (PermeableOptic k is b, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m (ViewResult k is b)
o <%= f = o %%= \a -> let b = f a in (b, b)

(<?=)
  :: (PermeableOptic k is (Maybe b), MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m (ViewResult k is (Maybe b))
o <?= b = o <.= Just b

(<.=)
  :: (PermeableOptic k is b, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m (ViewResult k is b)
o <.= b = o <%= const b

-------------------------------------------------------------------------------
-- Returning old value
-------------------------------------------------------------------------------

infix 4 <<.=, <<?=, <<%=

(<<%=)
  :: (PermeableOptic k is a, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m (ViewResult k is a)
o <<%= f = o %%= \a -> (a, f a)

(<<?=)
  :: (PermeableOptic k is (Maybe a), MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m (ViewResult k is (Maybe a))
o <<?= b = o <<.= Just b

(<<.=)
  :: (PermeableOptic k is a, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m (ViewResult k is a)
o <<.= b = o <<%= const b
