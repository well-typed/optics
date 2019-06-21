-- |
-- Module: Optics.State
-- Description: 'Setter' utilities for working with 'MonadState'.
--
module Optics.State
  ( modifying
  , modifying'
  , assign
  , assign'
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
  :: (Is k A_Setter, MonadState s m)
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
  :: (Is k A_Setter, MonadState s m)
  => Optic k is s s a b
  -> b
  -> m ()
assign' o = modifying' o . const
{-# INLINE assign' #-}

-- $setup
-- >>> import Data.Semigroup
