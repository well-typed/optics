{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Fold where

import Data.Monoid

import Optics.Internal.Bicontravariant
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Tag for a fold.
data A_Fold

-- | Constraints corresponding to a fold.
type instance Constraints A_Fold p =
  (Bicontravariant p, Cochoice p, Traversing p)

-- | Type synonym for a fold.
type Fold s a = Optic' A_Fold s a

-- | Fold to the first element (if it exists).
preview :: Is k A_Fold => Optic' k s a -> s -> Maybe a
preview o = getFirst #. foldMapOf o (First #. Just)
{-# INLINE preview #-}

-- | Explicitly cast an optic to a fold.
toFold :: Is k A_Fold => Optic' k s a -> Fold s a
toFold = sub
{-# INLINE toFold #-}

-- | Fold via embedding into a monoid.
foldMapOf :: (Monoid r, Is k A_Fold) => Optic' k s a -> (a -> r) -> s -> r
foldMapOf o =
  runForget . getOptic (toFold o) . Forget
{-# INLINE foldMapOf #-}

-- | Fold right-associatively.
foldrOf :: Is k A_Fold => Optic' k s a -> (a -> r -> r) -> r -> s -> r
foldrOf o arr r =
  (\ e -> appEndo e r) . foldMapOf o (Endo . arr)
{-# INLINE foldrOf #-}

-- | Fold left-associatively, and strictly.
foldlOf' :: Is k A_Fold => Optic' k s a -> (r -> a -> r) -> r -> s -> r
foldlOf' o rar r0 s =
  foldrOf o (\ a rr r -> rr $! rar r a) id s r0
{-# INLINE foldlOf' #-}

-- | Fold to a list.
toListOf :: Is k A_Fold => Optic' k s a -> s -> [a]
toListOf o = foldrOf o (:) []
{-# INLINE toListOf #-}
