-- |
-- Module: Optics.NeFold
-- Description: Extracts elements from a container.
--
-- A @'NeFold' S A@ has the ability to extract some non-zero number of elements of type @A@
-- from a container of type @S@.  For example, 'toNonEmptyOf' can be used to obtain
-- the contained elements as a non-empty list. Unlike a 'Optics.Traversal.Traversal',
-- there is no way to set or update elements.
--
-- This can be seen as a generalisation of 'foldMap1', where the type @S@ does
-- not need to be a type constructor with @A@ as the last parameter.
--
-- A close relative is the 'Optics.AffineFold.AffineFold', which is a 'Fold'
-- that contains at most one element. 'NeFold' containst at least one element.
--
module Optics.NeFold (
    -- * Formation
    NeFold

  -- * Introduction
  , foldrMapping1

  -- * Elimination
  -- , foldOf
  , foldMap1Of
  , foldrMap1Of
  -- , foldlOf'
  , toNonEmptyOf

  -- * Computation
  --
  -- |
  --
  -- @
  -- 'foldrMap1Of' ('foldrMapping1' f) ≡ f
  -- @ 

  -- * Additional introduction forms
  , folded1   
  , folding1
  , foldring

  -- * Additional elimination forms

  -- * Semigroup structure #monoids#
  -- | 'NeFold' admits (at least) one semigroups structures:
  --
  -- * 'summingL' (or 'summingR') concatenates results from both folds.
  --
  -- TODO: one can concatenate 'Fold' with 'NeFold' and still get 'NeFold'.
  , summingL
  , summingR

  -- * Subtyping
  , A_NeFold
  -- | <<diagrams/NeFold.png NeFold in the optics hierarchy>>
) where


import Data.Foldable1
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List.NonEmpty as NE

import Data.Profunctor.Indexed

import Optics.Fold
import Optics.Internal.Optic

-- | Type synonym for a non-empty fold.
type NeFold s a = Optic' A_NeFold NoIx s a

-- | Fold via embedding into a semigroup.
foldMap1Of :: (Is k A_NeFold, Semigroup m) => Optic' k is s a -> (a -> m) -> s -> m
foldMap1Of o = runForget #. getOptic (castOptic @A_NeFold o) .# Forget
{-# INLINE foldMap1Of #-}

-- | Fold right-associatively.
foldrMap1Of :: Is k A_NeFold => Optic' k is s a -> (a -> r) -> (a -> r -> r)  -> s -> r
foldrMap1Of o = \one arr s ->
    let h a Nothing = one a
        h a (Just b) = arr a b

    in appFromMaybe (foldMap1Of o (FromMaybe #. h) s) Nothing

{-# INLINE foldrMap1Of #-}

-- | Used for foldrMap1 and foldlMap1 definitions
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

{-
-- | Used for default toNonEmpty implementation.
newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  xs <> ys = NEDL (unNEDL xs . NE.toList . unNEDL ys)
  {-# INLINE (<>) #-}

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = NEDL #. (:|)

-- | Convert a dlist to a non-empty list
runNonEmptyDList :: NonEmptyDList a -> NonEmpty a
runNonEmptyDList = ($ []) . unNEDL
{-# INLINE runNonEmptyDList #-}
-}

-- | Fold to a non-empty list.
--
-- >>> toNonEmptyOf (_1 % folded1) ('h' :| ['i'], "bye")
-- 'h' :| "i"
toNonEmptyOf :: Is k A_NeFold => Optic' k is s a -> s -> NonEmpty a
toNonEmptyOf o = foldrMap1Of o (\a -> a :| []) NE.cons
{-# INLINE toNonEmptyOf #-}

----------------------------------------

-- | Fold via the 'Foldable1' class.
folded1 :: Foldable1 f => NeFold (f a) a
folded1 = Optic folded1__
{-# INLINE folded1 #-}

-- | Obtain a 'NeFold' by lifting an operation that returns a 'Foldable1' result.
--
-- >>> toListOf (folding tail) [1,2,3,4]
-- [2,3,4]
folding1 :: Foldable1 f => (s -> f a) -> NeFold s a
folding1 f = Optic (contrabimap f f . folded1__)
{-# INLINE folding1 #-}

-- | Obtain a 'NeFold' by lifting 'foldrMap1' like function.
--
-- >>> toListOf (foldring foldr) [1,2,3,4]
-- [1,2,3,4]
foldrMapping1
  :: (forall b. (a -> b) -> (a -> b -> b) ->  s -> b)
  -> NeFold s a
foldrMapping1 fr = Optic (foldrMapping1__ fr)
{-# INLINE foldrMapping1 #-}

-- | Return entries of the first 'NeFold', then the second one.
--
-- >>> toNonEmptyOf (_1 % folded `summingL` _2 % folded1) ([1,2], 4 :| [7,1])
-- 1 :| [2,4,7,1]
--
summingL
  :: (Is k A_Fold, Is l A_NeFold)
  => Optic' k is s a
  -> Optic' l js s a
  -> NeFold s a
summingL a b = foldrMapping1 $ \f g s -> foldrOf a g (foldrMap1Of b f g s) s
infixr 6 `summingL` -- Same as (<>)
{-# INLINE summingL #-}

-- | Return entries of the first 'NeFold', then the second one.
-- 
-- Use 'summingL' if you can.
--
-- >>> toNonEmptyOf (_1 % folded1 `summingR` _2 % folded2) (0 :| [1,2], 4 :| [7,1])
-- 0 :| [1,2,4,7,1]
--
summingR
  :: (Is k A_NeFold, Is l A_Fold)
  => Optic' k is s a
  -> Optic' l js s a
  -> NeFold s a
summingR a b = foldrMapping1 $ \f g s ->
    let tmp = foldrOf b (\x acc -> Just $ maybe (f x) (g x) acc) Nothing s
    in foldrMap1Of a (\x -> maybe (f x) (g x) tmp) g s 
infixr 6 `summingR` -- Same as (<>)
{-# INLINE summingR #-}
