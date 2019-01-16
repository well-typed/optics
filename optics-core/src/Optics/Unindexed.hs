{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TypeApplications #-}
module Optics.Unindexed where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Optic.TypeLevel

class UnindexableOptic k where
  -- | Downcast an indexed optic to its unindexed equivalent.
  unIx :: ConstN is => Optic k is s t a b -> Optic k '[] s t a b

instance UnindexableOptic A_Traversal where
  unIx :: forall is s t a b. ConstN is => Optic A_Traversal is s t a b -> Optic A_Traversal '[] s t a b
  unIx (Optic o) = Optic (ixcontramap (constN @is). o)
  {-# INLINE unIx #-}

instance UnindexableOptic A_Fold where
  unIx :: forall is s t a b. ConstN is => Optic A_Fold is s t a b -> Optic A_Fold '[] s t a b
  unIx (Optic o) = Optic (ixcontramap (constN @is). o)
  {-# INLINE unIx #-}

instance UnindexableOptic A_Setter where
  unIx :: forall is s t a b. ConstN is => Optic A_Setter is s t a b -> Optic A_Setter '[] s t a b
  unIx (Optic o) = Optic (ixcontramap (constN @is). o)
  {-# INLINE unIx #-}
