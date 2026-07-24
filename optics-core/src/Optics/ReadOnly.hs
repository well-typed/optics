-- |
-- Module: Optics.ReadOnly
-- Description: Converting read-write optics into their read-only counterparts.
--
-- This module defines 'getting', which turns a read-write optic into its
-- read-only counterpart.
--
module Optics.ReadOnly
  ( ToReadOnly(..)
  ) where

import Data.Profunctor.Indexed

import Optics.Internal.Bi
import Optics.Internal.Optic

-- | Class for read-write optics that have their read-only counterparts.
class ToReadOnly k s t a b where
  type ReadOnlyOptic k :: OpticKind
  -- | Turn read-write optic into its read-only counterpart (or leave read-only
  -- optics as-is).
  --
  -- This is useful when you have an @optic :: 'Optic' k is s t a b@ of read-write
  -- kind @k@ such that @s@, @t@, @a@, @b@ are rigid, there is no evidence that
  -- @s ~ t@ and @a ~ b@ and you want to pass @optic@ to one of the functions
  -- that accept read-only optic kinds.
  --
  -- Example:
  --
  -- >>> let fstIntToChar = _1 :: Lens (Int, r) (Char, r) Int Char
  --
  -- >>> :t view fstIntToChar
  -- ...
  -- ...Couldn't match type ‘Char’ with ‘Int’
  -- ...
  --
  -- >>> :t view (getting fstIntToChar)
  -- view (getting fstIntToChar) :: (Int, r) -> Int
  getting :: Optic k is s t a b -> Optic' (ReadOnlyOptic k) is s a

instance ToReadOnly An_Iso s t a b where
  type ReadOnlyOptic An_Iso = A_Getter
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Lens s t a b where
  type ReadOnlyOptic A_Lens = A_Getter
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Prism s t a b where
  type ReadOnlyOptic A_Prism = An_AffineFold
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly An_AffineTraversal s t a b where
  type ReadOnlyOptic An_AffineTraversal = An_AffineFold
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Traversal s t a b where
  type ReadOnlyOptic A_Traversal = A_Fold
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_ReversedPrism s t a b where
  type ReadOnlyOptic A_ReversedPrism = A_Getter
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance (s ~ t, a ~ b) => ToReadOnly A_Getter s t a b where
  type ReadOnlyOptic A_Getter = A_Getter
  getting = id
  {-# INLINE getting #-}

instance (s ~ t, a ~ b) => ToReadOnly An_AffineFold s t a b where
  type ReadOnlyOptic An_AffineFold = An_AffineFold
  getting = id
  {-# INLINE getting #-}

instance (s ~ t, a ~ b) => ToReadOnly A_Fold s t a b where
  type ReadOnlyOptic A_Fold = A_Fold
  getting = id
  {-# INLINE getting #-}

-- | Internal implementation of 'getting'.
getting__
  :: (Profunctor p, Bicontravariant p, Constraints k p)
  => Optic k            is    s t a b
  -> Optic__ p i (Curry is i) s s a a
getting__ (Optic o) = rphantom . o . rphantom
{-# INLINE getting__ #-}

-- $setup
-- >>> import Optics.Core
