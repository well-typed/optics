{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module: Optics.IxFold
-- Description: An indexed version of a 'Optics.Fold.Fold'.
--
-- An 'IxFold' is an indexed version of a 'Optics.Fold.Fold'. See the "Indexed
-- optics" section of the overview documentation in the @Optics@ module of the
-- main @optics@ package for more details on indexed optics.
--
module Optics.IxFold
  (
  -- * Formation
    IxFold

  -- * Introduction
  , ifoldVL

  -- * Elimination
  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf'
  , itoListOf
  , itraverseOf_
  , iforOf_

  -- * Additional introduction forms
  , ifolded
  , ifolding
  , ifoldring

  -- * Additional elimination forms
  -- | See also 'Data.Map.Optics.toMapOf', which constructs a 'Data.Map.Map' from an 'IxFold'.
  , iheadOf
  , ilastOf
  , ianyOf
  , iallOf
  , inoneOf
  , ifindOf
  , ifindMOf

  -- * Combinators
  , ipre
  , ifiltered
  , ibackwards_

  -- * Monoid structures #monoids#
  -- | 'IxFold' admits (at least) two monoid structures:
  --
  -- * 'isumming' concatenates results from both folds.
  --
  -- * 'ifailing' returns results from the second fold only if the first returns
  --   no results.
  --
  -- In both cases, the identity element of the monoid is
  -- `Optics.IxAffineTraversal.ignored`, which returns no results.
  --
  -- There is no 'Semigroup' or 'Monoid' instance for 'IxFold', because there is
  -- not a unique choice of monoid to use, and the ('<>') operator could not be
  -- used to combine optics of different kinds.
  , isumming
  , ifailing_

  -- * Subtyping
  , A_Fold

  -- * Re-exports
  , FoldableWithIndex(..)
  ) where

import Control.Applicative.Backwards
import Data.Monoid

import Data.Profunctor.Indexed

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Indexed.Classes
import Optics.Internal.Fold
import Optics.Internal.IxFold
import Optics.Internal.Optic
import Optics.Internal.Utils
import Optics.IxAffineFold
import Optics.Fold

-- | Type synonym for an indexed fold.
type IxFold i s a = Optic' A_Fold (WithIx i) s a

-- | Obtain an indexed fold by lifting 'itraverse_' like function.
--
-- @
-- 'ifoldVL' '.' 'itraverseOf_' ≡ 'id'
-- 'itraverseOf_' '.' 'ifoldVL' ≡ 'id'
-- @
ifoldVL
  :: (forall f. Applicative f => (i -> a -> f u) -> s -> f v)
  -> IxFold i s a
ifoldVL f = Optic (ifoldVL__ f)
{-# INLINE ifoldVL #-}

-- | Fold with index via embedding into a monoid.
ifoldMapOf
  :: (Is k A_Fold, Monoid m, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> m) -> s -> m
ifoldMapOf o = \f -> runIxForget (getOptic (castOptic @A_Fold o) (IxForget f)) id
{-# INLINE ifoldMapOf #-}

-- | Fold with index right-associatively.
ifoldrOf
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf o = \iarr r0 s -> (\e -> appEndo e r0) $ ifoldMapOf o (\i -> Endo #. iarr i) s
{-# INLINE ifoldrOf #-}

-- | Fold with index left-associatively, and strictly.
ifoldlOf'
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' o = \irar r0 s -> ifoldrOf o (\i a rr r -> rr $! irar i r a) id s r0
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> s -> [(i, a)]
itoListOf o = ifoldrOf o (\i -> (:) . (i, )) []
{-# INLINE itoListOf #-}

----------------------------------------

-- | Traverse over all of the targets of an 'IxFold', computing an
-- 'Applicative'-based answer, but unlike 'Optics.IxTraversal.itraverseOf' do
-- not construct a new structure.
--
-- >>> itraverseOf_ each (curry print) ("hello","world")
-- (0,"hello")
-- (1,"world")
--
itraverseOf_
  :: (Is k A_Fold, Applicative f, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> f r) -> s -> f ()
#if __GLASGOW_HASKELL__ == 802
-- GHC 8.2.2 needs this to optimize away profunctors when f is not supplied.
itraverseOf_ o = \f ->
#else
itraverseOf_ o f =
#endif
  runTraversed . ifoldMapOf o (\i -> Traversed #. f i)
{-# INLINE itraverseOf_ #-}

-- | A version of 'itraverseOf_' with the arguments flipped.
iforOf_
  :: (Is k A_Fold, Applicative f, is `HasSingleIndex` i)
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
ifolding f = Optic $ contrafirst f . ifolded__
{-# INLINE ifolding #-}

-- | Obtain an 'IxFold' by lifting 'ifoldr' like function.
--
-- >>> itoListOf (ifoldring ifoldr) "hello"
-- [(0,'h'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
ifoldring
  :: (forall f. Applicative f => (i -> a -> f u -> f u) -> f v -> s -> f w)
  -> IxFold i s a
ifoldring fr = Optic (ifoldring__ fr)
{-# INLINE ifoldring #-}

-- | Convert an indexed fold to an 'IxAffineFold' that visits the first element
-- of the original fold.
--
-- For the traversal version see 'Optics.IxTraversal.isingular'.
ipre
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> IxAffineFold i s a
ipre = iafolding . iheadOf
{-# INLINE ipre #-}

-- | Filter results of an 'IxFold' that don't satisfy a predicate.
--
-- >>> toListOf (ifolded %& ifiltered (>)) [3,2,1,0]
-- [1,0]
--
ifiltered
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => (i -> a -> Bool)
  -> Optic' k is s a
  -> IxFold i s a
ifiltered p o = ifoldVL $ \f ->
  itraverseOf_ o (\i a -> if p i a then f i a else pure ())
{-# INLINE ifiltered #-}
-- Note: technically this should be defined per optic kind:
--
-- ifiltered :: _ -> IxFold i s a       -> IxFold i s a
-- ifiltered :: _ -> IxGetter i s a     -> IxAffineFold i s a
-- ifiltered :: _ -> IxAffineFold i s a -> IxAffineFold i s a
--
-- and similarly for (non-existent) unsafeIFiltered.

-- | This allows you to traverse the elements of an 'IxFold' in the opposite
-- order.
ibackwards_
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> IxFold i s a
ibackwards_ o = conjoined (backwards_ o) $ ifoldVL $ \f ->
  forwards #. itraverseOf_ o (\i -> Backwards #. f i)
{-# INLINE ibackwards_ #-}

-- | Return entries of the first 'IxFold', then the second one.
--
-- >>> itoListOf (ifolded `isumming` ibackwards_ ifolded) ["a","b"]
-- [(0,"a"),(1,"b"),(1,"b"),(0,"a")]
--
-- For the traversal version see 'Optics.IxTraversal.iadjoin'.
isumming
  :: (Is k A_Fold, Is l A_Fold,
      is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic' k is1 s a
  -> Optic' l is2 s a
  -> IxFold i s a
isumming a b = conjoined (summing a b) $ ifoldVL $ \f s ->
  itraverseOf_ a f s *> itraverseOf_ b f s
infixr 6 `isumming` -- Same as (<>)
{-# INLINE isumming #-}

-- | Try the first 'IxFold'. If it returns no entries, try the second one.
--
-- >>> itoListOf (_1 % ifolded `ifailing` _2 % ifolded) (["a"], ["b","c"])
-- [(0,"a")]
-- >>> itoListOf (_1 % ifolded `ifailing` _2 % ifolded) ([], ["b","c"])
-- [(0,"b"),(1,"c")]
--
ifailing_
  :: (Is k A_Fold, Is l A_Fold, is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic' k is1 s a
  -> Optic' l is2 s a
  -> IxFold i s a
ifailing_ a b = conjoined (failing_ a b) $ ifoldVL $ \f s ->
  let OrT visited fu = itraverseOf_ a (\i -> wrapOrT . f i) s
  in if visited
     then fu
     else itraverseOf_ b f s
infixl 3 `ifailing_` -- Same as (<|>)
{-# INLINE ifailing_ #-}

----------------------------------------
-- Special folds

-- | Retrieve the first entry of an 'IxFold' along with its index.
--
-- >>> iheadOf ifolded [1..10]
-- Just (0,1)
iheadOf
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a -> s -> Maybe (i, a)
iheadOf o = getLeftmost . ifoldMapOf o (\i -> LLeaf . (i, ))
{-# INLINE iheadOf #-}

-- | Retrieve the last entry of an 'IxFold' along with its index.
--
-- >>> ilastOf ifolded [1..10]
-- Just (9,10)
ilastOf
  :: (Is k A_Fold, is `HasSingleIndex` i)
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a -> (i -> a -> Bool) -> s -> Bool
ianyOf o = \f -> getAny #. ifoldMapOf o (\i -> Any #. f i)
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a -> (i -> a -> Bool) -> s -> Bool
iallOf o = \f -> getAll #. ifoldMapOf o (\i -> All #. f i)
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a -> (i -> a -> Bool) -> s -> Bool
inoneOf o = \f -> not . ianyOf o f
{-# INLINE inoneOf #-}

-- | The 'ifindOf' function takes an 'IxFold', a predicate that is also supplied
-- the index, a structure and returns the left-most element of the structure
-- along with its index matching the predicate, or 'Nothing' if there is no such
-- element.
--
-- When you don't need access to the index then 'findOf' is more flexible in
-- what it accepts.
ifindOf
 :: (Is k A_Fold, is `HasSingleIndex` i)
 => Optic' k is s a -> (i -> a -> Bool) -> s -> Maybe (i, a)
ifindOf o = \p -> iheadOf (ifiltered p o)
{-# INLINE ifindOf #-}

-- | The 'ifindMOf' function takes an 'IxFold', a monadic predicate that is also
-- supplied the index, a structure and returns in the monad the left-most
-- element of the structure matching the predicate, or 'Nothing' if there is no
-- such element.
--
-- When you don't need access to the index then 'findMOf' is more flexible in
-- what it accepts.
ifindMOf
  :: (Is k A_Fold, Monad m, is `HasSingleIndex` i)
  => Optic' k is s a -> (i -> a -> m Bool) -> s -> m (Maybe (i, a))
ifindMOf o = \f -> ifoldrOf o
  (\i a y -> f i a >>= \r -> if r then pure (Just (i, a)) else y)
  (pure Nothing)
{-# INLINE ifindMOf #-}

-- $setup
-- >>> import Optics.Core
