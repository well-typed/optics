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
import Optics.Passthrough
import Optics.View

infix 4 .=, ?=, %=

-- | Replace the target(s) of an 'Optic' in our monadic state with a new value,
-- irrespective of the old.
--
-- This is an infix version of 'assign'.
(.=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m ()
o .= x = o %= const x
{-# INLINE (.=) #-}

-- | Replace the target(s) of an 'Optic' in our monadic state with 'Just' a new
-- value, irrespective of the old.
(?=)
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m ()
o ?= x = o %= const (Just x)
{-# INLINE (?=) #-}

-- | Map over the target(s) of an 'Optic' in our monadic state.
--
-- This is an infix version of 'modifying'.
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

-- | Modify the target of an 'PermeableOptic' in the current state returning
-- some extra information of type depending on the optic (@r@, @Maybe r@ or
-- monoidal summary).
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

-- | Modify the target of a 'PermeableOptic' into your 'Monad''s state by a user
-- supplied function and return the result.
(<%=)
  :: (PermeableOptic k b, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m (ViewResult k b)
o <%= f = o %%= \a -> let b = f a in (b, b)
{-# INLINE (<%=) #-}

-- | Set 'Just' a value with pass-through.
--
-- This is useful for chaining assignment without round-tripping through your
-- 'Monad' stack.
(<?=)
  :: (PermeableOptic k (Maybe b), MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m (ViewResult k (Maybe b))
o <?= b = o <.= Just b
{-# INLINE (<?=) #-}

-- | Set with pass-through.
--
-- This is useful for chaining assignment without round-tripping through your
-- 'Monad' stack.
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

-- | Modify the target of a 'PermeableOptic' into your 'Monad''s state by a user
-- supplied function and return the /old/ value that was replaced.
(<<%=)
  :: (PermeableOptic k a, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m (ViewResult k a)
o <<%= f = o %%= \a -> (a, f a)
{-# INLINE (<<%=) #-}

-- | Replace the target of a 'PermeableOptic' into your 'Monad''s state with
-- 'Just' a user supplied value and return the /old/ value that was replaced.
(<<?=)
  :: (PermeableOptic k (Maybe a), MonadState s m)
  => Optic k is s s (Maybe a) (Maybe b)
  -> b
  -> m (ViewResult k (Maybe a))
o <<?= b = o <<.= Just b
{-# INLINE (<<?=) #-}

-- | Replace the target of a 'PermeableOptic' into your 'Monad''s state with a
-- user supplied value and return the /old/ value that was replaced.
(<<.=)
  :: (PermeableOptic k a, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m (ViewResult k a)
o <<.= b = o <<%= const b
{-# INLINE (<<.=) #-}
