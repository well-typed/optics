module Optics.Unindexed where

import Optics.Internal.Optic

import Optics.Internal.Setter
import Optics.Internal.Fold
import Optics.Internal.Traversal

class NoIxOptic k s t a b where
  -- | Downcast an indexed optic to its unindexed equivalent.
  noIx :: Optic k is s t a b -> Optic k NoIx s t a b

instance NoIxOptic A_Traversal s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = traversalVL (traverseOf o)
  {-# INLINE noIx #-}

instance (s ~ t, a ~ b) => NoIxOptic A_Fold s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = foldVL (traverseOf_ o)
  {-# INLINE noIx #-}

instance NoIxOptic A_Setter s t a b where
  -- Reinterpret the optic as unindexed one for conjoined to work.
  noIx o = sets (over o)
  {-# INLINE noIx #-}
