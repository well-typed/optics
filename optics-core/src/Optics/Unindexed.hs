module Optics.Unindexed where

import Optics.Internal.Optic

class UnindexableOptic k where
  type UnindexedOptic k :: OpticKind
  -- | Downcast an indexed optic to its unindexed equivalent.
  unIx :: Optic k i o s t a b -> Optic (UnindexedOptic k) i o s t a b

instance UnindexableOptic An_IxTraversal where
  type UnindexedOptic An_IxTraversal = A_Traversal
  unIx (Optic o) = Optic o
  {-# INLINE unIx #-}

instance UnindexableOptic An_IxFold where
  type UnindexedOptic An_IxFold = A_Fold
  unIx (Optic o) = Optic o
  {-# INLINE unIx #-}

instance UnindexableOptic An_IxSetter where
  type UnindexedOptic An_IxSetter = A_Setter
  unIx (Optic o) = Optic o
  {-# INLINE unIx #-}
