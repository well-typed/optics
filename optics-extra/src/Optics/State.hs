-- |
-- Module: Optics.State
-- Description: 'Setter' utilities for working with 'MonadState'.
--
-- This module contains utilities for working with 'Setter's in a 'MonadState'
-- context.  If you prefer operator versions, you may wish to import
-- "Optics.State.Operators".
--
module Optics.State
  ( modifying
  , modifying'
  , assign
  , assign'
  , use
  , preuse
  ) where

import Control.Monad.State

import Optics.Core

-- | Map over the target(s) of an 'Optic' in our monadic state.
--
-- >>> execState (do modifying _1 (*10); modifying _2 $ stimes 5) (6,"o")
-- (60,"ooooo")
--
-- >>> execState (modifying each $ stimes 2) ("a","b")
-- ("aa","bb")
modifying
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m ()
modifying o = modify . over o
{-# INLINE modifying #-}

-- | Version of 'modifying' that is strict in both optic application and state
-- modification.
--
-- >>> flip evalState ('a','b') $ modifying _1 (errorWithoutStackTrace "oops")
-- ()
--
-- >>> flip evalState ('a','b') $ modifying' _1 (errorWithoutStackTrace "oops")
-- *** Exception: oops
modifying'
  :: (Is k A_Traversal, MonadState s m)
  => Optic k is s s a b
  -> (a -> b)
  -> m ()
modifying' o = modify' . over' o
{-# INLINE modifying' #-}

-- | Replace the target(s) of an 'Optic' in our monadic state with a new value,
-- irrespective of the old.
--
-- >>> execState (do assign _1 'c'; assign _2 'd') ('a','b')
-- ('c','d')
--
-- >>> execState (assign each 'c') ('a','b')
-- ('c','c')
assign
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m ()
assign o = modifying o . const
{-# INLINE assign #-}

-- | Version of 'assign' that is strict in both optic application and state
-- modification.
--
-- >>> flip evalState ('a','b') $ assign _1 (errorWithoutStackTrace "oops")
-- ()
--
-- >>> flip evalState ('a','b') $ assign' _1 (errorWithoutStackTrace "oops")
-- *** Exception: oops
assign'
  :: (Is k A_Traversal, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m ()
assign' o = modifying' o . const
{-# INLINE assign' #-}

-- | Use the target of a 'Lens', 'Iso', or 'Getter' in the current state.
--
-- >>> evalState (use _1) ('a','b')
-- 'a'
--
-- >>> evalState (use _2) ("hello","world")
-- "world"
--
use
  :: (Is k A_Getter, MonadState s m)
  => Optic' k is s a
  -> m a
use o = gets (view o)
{-# INLINE use #-}

-- | Use the target of a 'AffineTraveral' or 'AffineFold' in the current state.
--
-- >>> evalState (preuse $ _1 % _Right) (Right 'a','b')
-- Just 'a'
--
-- @since 0.2
preuse
  :: (Is k An_AffineFold, MonadState s m)
  => Optic' k is s a
  -> m (Maybe a)
preuse o = gets (preview o)
{-# INLINE preuse #-}

-- $setup
-- >>> import Data.Semigroup
