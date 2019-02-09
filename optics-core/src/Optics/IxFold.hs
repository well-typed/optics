{-# LANGUAGE DataKinds #-}
module Optics.IxFold
  ( A_Fold
  , IxFold
  , toIxFold
  , ixFoldVL
  , conjoinedFold
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf'
  , itoListOf
  , itraverseOf_
  , iforOf_
  -- * Folds
  , ifolded
  , ifolding
  , ifoldring
  , ifiltered
  , ibackwards_
  -- * Special folds
  , iheadOf
  , ilastOf
  , ianyOf
  , iallOf
  , inoneOf
  , ifindOf
  , ifindMOf
  , module Optics.Optic
  ) where

import Control.Applicative.Backwards
import Data.Foldable
import Data.Monoid

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Fold
import Optics.Internal.IxFold
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils
import Optics.Fold
import Optics.Optic

-- | Type synonym for an indexed fold.
type IxFold i s a = Optic' A_Fold (WithIx i) s a

-- | Explicitly cast an optic to an indexed fold.
toIxFold :: Is k A_Fold => Optic' k (WithIx i) s a -> IxFold i s a
toIxFold = castOptic
{-# INLINE toIxFold #-}

-- | Build an indexed fold from the "almost van Laarhoven" representation.
--
-- @
-- 'ixFoldVL' '.' 'itraverseOf_' ≡ 'id'
-- 'itraverseOf_' '.' 'ixFoldVL' ≡ 'id'
-- @
ixFoldVL
  :: (forall f. Applicative f => (i -> a -> f r) -> s -> f ())
  -> IxFold i s a
ixFoldVL f = Optic (ixFoldVL__ f)
{-# INLINE ixFoldVL #-}

-- | Build an indexed fold from the van Laarhoven representation of both its
-- unindexed and indexed version.
--
-- Appropriate version of the fold will be automatically picked for maximum
-- efficiency depending on whether it is used as indexed or regular one.
--
-- @
-- 'traverseOf_'  ('conjoinedFold' f g) ≡ 'traverseOf_'  ('foldVL' f)
-- 'itraverseOf_' ('conjoinedFold' f g) ≡ 'itraverseOf_' ('ixFoldVL' g)
-- @
conjoinedFold
  :: (forall f. Applicative f => (     a -> f r) -> s -> f ())
  -> (forall f. Applicative f => (i -> a -> f r) -> s -> f ())
  -> IxFold i s a
conjoinedFold f g = Optic (conjoinedFold__ f g)
{-# INLINE conjoinedFold #-}

-- | Fold with index via embedding into a monoid.
ifoldMapOf
  :: (Is k A_Fold, Monoid m, (is `HasSingleIndex` i) "ifoldMapOf" 1)
  => Optic' k is s a
  -> (i -> a -> m) -> s -> m
ifoldMapOf o f = runIxForget (getOptic (toIxFold o) (IxForget f)) id
{-# INLINE ifoldMapOf #-}

-- | Fold with index right-associatively.
ifoldrOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ifoldrOf" 1)
  => Optic' k is s a
  -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf o iarr r0 s = (\e -> appEndo e r0) $ ifoldMapOf o (\i -> Endo #. iarr i) s
{-# INLINE ifoldrOf #-}

-- | Fold with index left-associatively, and strictly.
ifoldlOf'
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ifoldlOf'" 1)
  => Optic' k is s a
  -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' o irar r0 s = ifoldrOf o (\i a rr r -> rr $! irar i r a) id s r0
{-# INLINE ifoldlOf' #-}

-- | Fold with index to a list.
--
-- >>> itoListOf (folded % ifolded) ["abc", "def"]
-- [(0,'a'),(1,'b'),(2,'c'),(0,'d'),(1,'e'),(2,'f')]
--
-- /Note:/ currently indexed optics can be used as non-indexed.
--
-- >>> toListOf (folded % ifolded) ["abc", "def"]
-- "abcdef"
--
itoListOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "itoListOf" 1)
  => Optic' k is s a
  -> s -> [(i, a)]
itoListOf o = ifoldrOf o (\i -> (:) . (i, )) []
{-# INLINE itoListOf #-}

----------------------------------------

itraverseOf_
  :: (Is k A_Fold, Applicative f, (is `HasSingleIndex` i) "itraverseOf_" 1)
  => Optic' k is s a
  -> (i -> a -> f r) -> s -> f ()
itraverseOf_ o = \f ->
  -- Need to have eta-expansion here for GHC 8.2.2 to properly optimize away the
  -- profunctor stuff when f is not supplied.
  runTraversed . ifoldMapOf o (\i -> Traversed #. f i)
{-# INLINE itraverseOf_ #-}

-- | A version of 'itraverseOf_' with the arguments flipped.
iforOf_
  :: (Is k A_Fold, Applicative f, (is `HasSingleIndex` i) "iforOf_" 1)
  => Optic' k is s a
  -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

----------------------------------------

-- | Indexed fold via 'FoldableWithIndex' class.
ifolded :: FoldableWithIndex i f => IxFold i (f a) a
ifolded = Optic ifolded__
{-# INLINE ifolded #-}

-- | Obtain an 'IxFold' by lifting an operation that returns a
-- 'FoldableWithIndex' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into an
-- 'IxFold'.
--
-- >>> itoListOf (ifolding words) "how are you"
-- [(0,"how"),(1,"are"),(2,"you")]
ifolding :: FoldableWithIndex i f => (s -> f a) -> IxFold i s a
ifolding f = Optic $ contrabimap f (\_ -> ())
                   . conjoinedFold__ traverse_ itraverse_
{-# INLINE ifolding #-}

-- | Obtain an 'IxFold' by lifting 'ifoldr' like function.
--
-- >>> itoListOf (ifoldring ifoldr) "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
ifoldring
  :: (forall f. Applicative f => (i -> a -> f r -> f r) -> f r -> s -> f r)
  -> IxFold i s a
ifoldring fr = Optic (ifoldring__ fr)
{-# INLINE ifoldring #-}

-- | Filter results of an 'IxFold' that don't satisfy a predicate.
ifiltered
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ifiltered" 1)
  => (i -> a -> Bool)
  -> Optic' k is s a
  -> IxFold i s a
ifiltered p o = ixFoldVL $ \f ->
  itraverseOf_ o (\i a -> if p i a then f i a else pure ())
{-# INLINE ifiltered #-}

-- | This allows you to traverse the elements of an 'IxFold' in the opposite
-- order.
ibackwards_
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ibackwards_" 1)
  => Optic' k is s a
  -> IxFold i s a
ibackwards_ o = conjoinedFold
  (\f -> forwards #. traverseOf_  o (Backwards #. f))
  (\f -> forwards #. itraverseOf_ o (\i -> Backwards #. f i))
{-# INLINE ibackwards_ #-}

----------------------------------------
-- Special folds

-- | Retrieve the first entry of an 'IxFold' along with its index.
--
-- >>> iheadOf ifolded [1..10]
-- Just (0,1)
iheadOf
  ::( Is k A_Fold, (is `HasSingleIndex` i) "ifirstOf" 1)
  => Optic' k is s a -> s -> Maybe (i, a)
iheadOf o = getLeftmost . ifoldMapOf o (\i -> LLeaf . (i, ))
{-# INLINE iheadOf #-}

-- | Retrieve the last entry of an 'IxFold' along with its index.
--
-- >>> ilastOf ifolded [1..10]
-- Just (9,10)
ilastOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ilastOf" 1)
  => Optic' k is s a -> s -> Maybe (i, a)
ilastOf o = getRightmost . ifoldMapOf o (\i -> RLeaf . (i, ))
{-# INLINE ilastOf #-}

-- | Return whether or not any element viewed through an 'IxFold' satisfies a
-- predicate, with access to the @i@.
--
-- When you don't need access to the index then 'anyOf' is more flexible in what
-- it accepts.
--
-- @
-- 'anyOf' o ≡ 'ianyOf' o '.' 'const'
-- @
ianyOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "ianyOf" 1)
  => Optic' k is s a -> (i -> a -> Bool) -> s -> Bool
ianyOf o f = getAny #. ifoldMapOf o (\i -> Any #. f i)
{-# INLINE ianyOf #-}

-- | Return whether or not all elements viewed through an 'IxFold' satisfy a
-- predicate, with access to the @i@.
--
-- When you don't need access to the index then 'allOf' is more flexible in what
-- it accepts.
--
-- @
-- 'allOf' o ≡ 'iallOf' o '.' 'const'
-- @
iallOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "iallOf" 1)
  => Optic' k is s a -> (i -> a -> Bool) -> s -> Bool
iallOf o f = getAll #. ifoldMapOf o (\i -> All #. f i)
{-# INLINE iallOf #-}

-- | Return whether or not none of the elements viewed through an 'IxFold'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'noneOf' is more flexible in
-- what it accepts.
--
-- @
-- 'noneOf' o ≡ 'inoneOf' o '.' 'const'
-- @
inoneOf
  :: (Is k A_Fold, (is `HasSingleIndex` i) "inoneOf" 1)
  => Optic' k is s a -> (i -> a -> Bool) -> s -> Bool
inoneOf o f = not . ianyOf o f
{-# INLINE inoneOf #-}

-- | The 'ifindOf' function takes an 'IxFold', a predicate that is also supplied
-- the index, a structure and returns the left-most element of the structure
-- along with its index matching the predicate, or 'Nothing' if there is no such
-- element.
--
-- When you don't need access to the index then 'findOf' is more flexible in
-- what it accepts.
ifindOf
 :: (Is k A_Fold, (is `HasSingleIndex` i) "ifindOf" 1)
 => Optic' k is s a -> (i -> a -> Bool) -> s -> Maybe (i, a)
ifindOf o p = iheadOf (ifiltered p o)
{-# INLINE ifindOf #-}

-- | The 'ifindMOf' function takes an 'IxFold', a monadic predicate that is also
-- supplied the index, a structure and returns in the monad the left-most
-- element of the structure matching the predicate, or 'Nothing' if there is no
-- such element.
--
-- When you don't need access to the index then 'findMOf' is more flexible in
-- what it accepts.
ifindMOf
  :: (Is k A_Fold, Monad m, (is `HasSingleIndex` i) "ifindMOf" 1)
  => Optic' k is s a -> (i -> a -> m Bool) -> s -> m (Maybe (i, a))
ifindMOf o f = ifoldrOf o
  (\i a y -> f i a >>= \r -> if r then pure (Just (i, a)) else y)
  (pure Nothing)
{-# INLINE ifindMOf #-}
