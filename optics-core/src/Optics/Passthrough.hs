{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.Passthrough where

import Control.Arrow (first)
import Data.Monoid

import Optics.AffineTraversal
import Optics.IxTraversal
import Optics.Lens
import Optics.Prism
import Optics.Traversal
import Optics.View
import Optics.Internal.Optic

class (Is k A_Traversal, ViewableOptic k is r) => PermeableOptic k is r where
  -- | Modify the target of an 'Optic' returning some extra information of type 'r'.
  passthrough
    :: Optic k is s t a b
    -> (a -> (r, b))
    -> s
    -> (ViewResult k is r, t)

instance is ~ NoIx => PermeableOptic An_Iso is r where
  passthrough = toLensVL
  {-# INLINE passthrough #-}

instance is ~ NoIx => PermeableOptic A_Lens is r where
  passthrough = toLensVL
  {-# INLINE passthrough #-}

instance is ~ NoIx => PermeableOptic A_Prism is r where
  passthrough o f s = withPrism o $ \bt sta -> case sta s of
    Left t -> (Nothing, t)
    Right a -> case f a of
      (r, b) -> (Just r, bt b)
  {-# INLINE passthrough #-}

instance is ~ NoIx => PermeableOptic An_AffineTraversal is r where
  passthrough o f s = withAffineTraversal o $ \sbt sta -> case sta s of
    Left t -> (Nothing, t)
    Right a -> case f a of
      (r, b) -> (Just r, sbt s b)
  {-# INLINE passthrough #-}

instance PermeableOptic A_Traversal NoIx r where
  passthrough o f = first (`appEndo` [])
    . traverseOf o (first (\r -> Endo (r :)) . f)
  {-# INLINE passthrough #-}

instance (CheckIndices "passthrough" 1 i (i ': is)
         ) => PermeableOptic A_Traversal (i ': is) r where
  passthrough o f = first (`appEndo` [])
    . itraverseOf o (\i -> first (\r -> Endo ((i, r) :)) . f)
  {-# INLINE passthrough #-}
