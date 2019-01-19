module Optics.Internal.Fold where

import Data.Foldable
import Data.Monoid

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a fold.
type Fold s a = Optic' A_Fold NoIx s a

-- | Explicitly cast an optic to a fold.
toFold
  :: Is k A_Fold
  => Optic' k is s a
  -> Optic' A_Fold is s a
toFold = castOptic
{-# INLINE toFold #-}

-- | View the result of folding over all the results of a 'Fold' or 'Traversal'
-- that points at a monoidal value.
viewN :: (Is k A_Fold, Monoid a) => Optic' k is s a -> s -> a
viewN o = runForget (getOptic (toFold o) (Forget id))
{-# INLINE viewN #-}

-- | Fold to the first element (if it exists).
preview :: Is k A_Fold => Optic' k is s a -> s -> Maybe a
preview o = getFirst #. foldMapOf o (First #. Just)
{-# INLINE preview #-}

-- | Create a 'Fold' from its "almost van Laarhoven" representation.
foldVL
  :: (forall f. Applicative f => (a -> f ()) -> s -> f ())
  -> Fold s a
foldVL f = Optic (contrasecond (\_ -> ()) . wander f . dimap id (\_ -> ()))
{-# INLINE foldVL #-}

-- | Fold via embedding into a monoid.
foldMapOf :: (Monoid r, Is k A_Fold) => Optic' k is s a -> (a -> r) -> s -> r
foldMapOf o = runForget #. getOptic (toFold o) .# Forget
{-# INLINE foldMapOf #-}

-- | Fold right-associatively.
foldrOf :: Is k A_Fold => Optic' k is s a -> (a -> r -> r) -> r -> s -> r
foldrOf o arr r = (\e -> appEndo e r) . foldMapOf o (Endo #. arr)
{-# INLINE foldrOf #-}

-- | Fold left-associatively, and strictly.
foldlOf' :: Is k A_Fold => Optic' k is s a -> (r -> a -> r) -> r -> s -> r
foldlOf' o rar r0 s = foldrOf o (\a rr r -> rr $! rar r a) id s r0
{-# INLINE foldlOf' #-}

-- | Fold to a list.
toListOf :: Is k A_Fold => Optic' k is s a -> s -> [a]
toListOf o = foldrOf o (:) []
{-# INLINE toListOf #-}

----------------------------------------

-- | Evaluate each applicative action referenced by a 'Fold' on the structure
-- from left to right, and ignore the results.
sequenceOf_
  :: (Is k A_Fold, Applicative f)
  => Optic' k is s (f a)
  -> s -> f ()
sequenceOf_ o = runTraversed . foldMapOf o Traversed
{-# INLINE sequenceOf_ #-}

-- | Traverse over all of the targets of an optic, computing an 'Applicative'
-- (or 'Functor')-based answer, but unlike 'Optics.Traversal.traverseOf' do not
-- construct a new structure.
--
-- 'traverseOf_' generalizes 'Data.Foldable.traverse_' to work over any 'Fold'.
--
traverseOf_
  :: (Is k A_Fold, Applicative f)
  => Optic' k is s a
  -> (a -> f r) -> s -> f ()
traverseOf_ o f = runTraversed . foldMapOf o (Traversed #. f)
{-# INLINE traverseOf_ #-}

----------------------------------------

-- | Fold via the 'Foldable' class.
folded :: Foldable f => Fold (f a) a
folded = Optic (contrasecond (\_ -> ()) . wander traverse_)
{-# INLINE folded #-}

-- | Obtain a 'Fold' by lifting an operation that returns a 'Foldable' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a
-- 'Fold'.
--
-- >>> toListOf (folding tail) [1,2,3,4]
-- [2,3,4]
folding :: Foldable f => (s -> f a) -> Fold s a
folding f = Optic (contrabimap f (\_ -> ()) . wander traverse_)
{-# INLINE folding #-}
