{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Fold where

import Control.Applicative (Const(..))
import Data.Monoid

import Optics.Internal.Contravariant
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a fold.
data A_Fold

-- | Constraints corresponding to a fold.
type instance Constraints A_Fold p f =
  (p ~ (->), Contravariant f, Applicative f)

-- | Type synonym for a fold.
type Fold s a = Optic' A_Fold s a

-- | Explicitly cast an optic to a fold.
toFold :: Is k A_Fold => Optic' k s a -> Fold s a
toFold = sub
{-# INLINE toFold #-}

-- | Build a fold from the van Laarhoven representation.
vlFold :: (forall f . (Applicative f, Contravariant f) => (a -> f a) -> s -> f s) -> Fold s a
vlFold = Optic
{-# INLINE vlFold #-}

-- | Fold via embedding into a monoid.
foldMapOf :: (Monoid r, Is k A_Fold) => Optic' k s a -> (a -> r) -> s -> r
foldMapOf o ar =
  getConst . getOptic (toFold o) (Const . ar)
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

-- | Fold to the first element (if it exists).
preview :: Is k A_Fold => Optic' k s a -> s -> Maybe a
preview o = foldrOf o (const . Just) Nothing
{-# INLINE preview #-}
