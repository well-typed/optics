module Optics.Unindexed where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Indexed

class UnindexableOptic k where
  -- | Downcast an indexed optic to its unindexed equivalent.
  unIx :: CheckIndices i o => Optic k i o s t a b -> Optic k i i s t a b

instance UnindexableOptic A_Traversal where
  unIx (Optic o) = Optic (ixcontramap const . o)
  {-# INLINE unIx #-}

instance UnindexableOptic A_Fold where
  unIx (Optic o) = Optic (ixcontramap const . o)
  {-# INLINE unIx #-}

instance UnindexableOptic A_Setter where
  unIx (Optic o) = Optic (ixcontramap const . o)
  {-# INLINE unIx #-}
