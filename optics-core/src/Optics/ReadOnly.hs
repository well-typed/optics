module Optics.ReadOnly
  ( ToReadOnly(..)
  ) where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Class for read-write optics that have their read-only counterparts.
class ToReadOnly k where
  type ReadOnlyOptic k
  -- | Turn read-write optic into its read-only counterpart.
  getting :: Optic k is s t a b -> Optic' (ReadOnlyOptic k) is s a

instance ToReadOnly An_Iso where
  type ReadOnlyOptic An_Iso = A_Getter
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Lens where
  type ReadOnlyOptic A_Lens = A_Getter
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Prism where
  type ReadOnlyOptic A_Prism = An_AffineFold
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly An_AffineTraversal where
  type ReadOnlyOptic An_AffineTraversal = An_AffineFold
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Traversal where
  type ReadOnlyOptic A_Traversal = A_Fold
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

-- | Internal implementation of 'getting'.
getting__
  :: (Profunctor p, Bicontravariant p, Constraints k p)
  => Optic k            is    s t a b
  -> Optic__ p i (Curry is i) s s a a
getting__ (Optic o) = rphantom . o . rphantom
{-# INLINE getting__ #-}
