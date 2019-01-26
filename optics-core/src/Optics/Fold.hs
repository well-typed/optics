module Optics.Fold
  ( A_Fold
  , Fold
  , toFold
  , viewN
  , preview
  , foldVL
  , foldMapOf
  , foldrOf
  , foldlOf'
  , toListOf
  , sequenceOf_
  , traverseOf_
  , forOf_
  , folded
  , folding
    -- * Concrete folds
  , has
  , andOf
  , orOf
  , allOf
  , anyOf
  , noneOf
  , productOf
  , sumOf
  , asumOf
  , msumOf
  , elemOf
  , notElemOf
  , concatOf
  , concatMapOf
  , lengthOf
  , module Optics.Optic
  )
  where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils
import Optics.Optic

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

-- | Build a 'Fold' from the "almost van Laarhoven" representation.
--
-- @
-- 'foldVL' '.' 'traverseOf_' ≡ 'id'
-- 'traverseOf_' '.' 'foldVL' ≡ 'id'
-- @
foldVL
  :: (forall f. Applicative f => (a -> f r) -> s -> f ())
  -> Fold s a
foldVL f = Optic (rphantom . wander f . rphantom)
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
traverseOf_ o = \f -> runTraversed . foldMapOf o (Traversed #. f)
{-# INLINE traverseOf_ #-}

-- | A version of 'traverseOf_' with the arguments flipped.
forOf_
  :: (Is k A_Fold, Applicative f)
  => Optic' k is s a
  -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

----------------------------------------

-- | Fold via the 'Foldable' class.
folded :: Foldable f => Fold (f a) a
folded = foldVL traverse_
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

----------------------------------------
-- Concrete folds

-- | Check to see if this optic matches 1 or more entries.
--
-- >>> has _Left (Left 12)
-- True
--
-- >>> has _Right (Left 12)
-- False
--
-- This will always return 'True' for a 'Lens' or 'Getter'.
--
-- >>> has _1 ("hello","world")
-- True
has :: Is k A_Fold => Optic' k is s a -> s -> Bool
has l = getAny #. foldMapOf l (\_ -> Any True)

andOf :: Is k A_Fold => Optic' k is s Bool -> s -> Bool
andOf o = getAll #. foldMapOf o All
{-# INLINE andOf #-}

orOf :: Is k A_Fold => Optic' k is s Bool -> s -> Bool
orOf o = getAny #. foldMapOf o Any
{-# INLINE orOf #-}

allOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
allOf o f = getAll #. foldMapOf o (All #. f)
{-# INLINE allOf #-}

anyOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
anyOf o f = getAny #. foldMapOf o (Any #. f)
{-# INLINE anyOf #-}

noneOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
noneOf o f = not . anyOf o f
{-# INLINE noneOf #-}

productOf :: (Is k A_Fold, Num a) => Optic' k is s a -> s -> a
productOf o = foldlOf' o (*) 1
{-# INLINE productOf #-}

sumOf :: (Is k A_Fold, Num a) => Optic' k is s a -> s -> a
sumOf o = foldlOf' o (+) 0
{-# INLINE sumOf #-}

asumOf :: (Is k A_Fold, Alternative f) => Optic' k is s (f a) -> s -> f a
asumOf o = foldrOf o (<|>) empty
{-# INLINE asumOf #-}

msumOf :: (Is k A_Fold, MonadPlus m) => Optic' k is s (m a) -> s -> m a
msumOf o = foldrOf o mplus mzero
{-# INLINE msumOf #-}

elemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
elemOf o = anyOf o . (==)
{-# INLINE elemOf #-}

notElemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
notElemOf o = allOf o . (/=)
{-# INLINE notElemOf #-}

concatOf :: Is k A_Fold => Optic' k is s [a] -> s -> [a]
concatOf o = foldMapOf o id
{-# INLINE concatOf #-}

concatMapOf :: Is k A_Fold => Optic' k is s a -> (a -> [b]) -> s -> [b]
concatMapOf = foldMapOf
{-# INLINE concatMapOf #-}

lengthOf :: Is k A_Fold => Optic' k is s a -> s -> Int
lengthOf o = foldlOf' o (\ n _ -> 1 + n) 0
{-# INLINE lengthOf #-}
