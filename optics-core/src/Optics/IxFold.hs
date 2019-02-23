{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
module Optics.IxFold
  ( A_Fold
  , IxFold
  , toIxFold
  , mkIxFold
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
  -- * Semigroup structure
  , isumming
  , ifailing
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

-- | Obtain an indexed fold by lifting 'itraverse_' like function.
--
-- @
-- 'mkIxFold' '.' 'itraverseOf_' ≡ 'id'
-- 'itraverseOf_' '.' 'mkIxFold' ≡ 'id'
-- @
mkIxFold
  :: (forall f. Applicative f => (i -> a -> f u) -> s -> f v)
  -> IxFold i s a
mkIxFold f = Optic (mkIxFold__ f)
{-# INLINE mkIxFold #-}

-- | Build an indexed fold from the van Laarhoven representation of both its
-- unindexed and indexed version.
--
-- Appropriate version of the fold will be automatically picked for maximum
-- efficiency depending on whether it is used as indexed or regular one.
--
-- @
-- 'traverseOf_'  ('conjoinedFold' f g) ≡ 'traverseOf_'  ('mkFold' f)
-- 'itraverseOf_' ('conjoinedFold' f g) ≡ 'itraverseOf_' ('mkIxFold' g)
-- @
conjoinedFold
  :: (forall f. Applicative f => (     a -> f u) -> s -> f v)
  -> (forall f. Applicative f => (i -> a -> f u) -> s -> f v)
  -> IxFold i s a
conjoinedFold f g = Optic (conjoinedFold__ f g)
{-# INLINE conjoinedFold #-}

-- | Fold with index via embedding into a monoid.
ifoldMapOf
  :: (Is k A_Fold, Monoid m, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> m) -> s -> m
ifoldMapOf o f = runIxForget (getOptic (toIxFold o) (IxForget f)) id
{-# INLINE ifoldMapOf #-}

-- | Fold with index right-associatively.
ifoldrOf
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf o iarr r0 s = (\e -> appEndo e r0) $ ifoldMapOf o (\i -> Endo #. iarr i) s
{-# INLINE ifoldrOf #-}

-- | Fold with index left-associatively, and strictly.
ifoldlOf'
  :: (Is k A_Fold, is `HasSingleIndex` i)
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> s -> [(i, a)]
itoListOf o = ifoldrOf o (\i -> (:) . (i, )) []
{-# INLINE itoListOf #-}

----------------------------------------

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
ifolding f = Optic $ contrafirst f . conjoinedFold__ traverse_ itraverse_
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

-- | Filter results of an 'IxFold' that don't satisfy a predicate.
ifiltered
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => (i -> a -> Bool)
  -> Optic' k is s a
  -> IxFold i s a
ifiltered p o = mkIxFold $ \f ->
  itraverseOf_ o (\i a -> if p i a then f i a else pure ())
{-# INLINE ifiltered #-}

-- | This allows you to traverse the elements of an 'IxFold' in the opposite
-- order.
ibackwards_
  :: (Is k A_Fold, is `HasSingleIndex` i)
  => Optic' k is s a
  -> IxFold i s a
ibackwards_ o = conjoinedFold
  (\f -> forwards #. traverseOf_  o (Backwards #. f))
  (\f -> forwards #. itraverseOf_ o (\i -> Backwards #. f i))
{-# INLINE ibackwards_ #-}

-- | Return entries of the first 'IxFold', then the second one.
isumming
  :: (Is k A_Fold, Is l A_Fold,
      is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic' k is1 s a
  -> Optic' l is2 s a
  -> IxFold i s a
isumming a b = conjoinedFold
  (\f s ->  traverseOf_ a f s *>  traverseOf_ b f s)
  (\f s -> itraverseOf_ a f s *> itraverseOf_ b f s)
infixr 6 `isumming` -- Same as (<>)
{-# INLINE isumming #-}

-- | Try the first 'IxFold'. If it returns no entries, try the second one.
ifailing
  :: forall k l is1 is2 i s a
  .  (Is k A_Fold, Is l A_Fold,
      is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic' k is1 s a
  -> Optic' l is2 s a
  -> IxFold i s a
ifailing a b = conjoinedFold noix withix
  where
    noix :: Applicative f => (a -> f r) -> s -> f ()
    noix f s =
      let OrT visited fu = traverseOf_ a (wrapOrT . f) s
      in if visited
         then fu
         else traverseOf_ b f s

    withix :: Applicative f => (i -> a -> f r) -> s -> f ()
    withix f s =
      let OrT visited fu = itraverseOf_ a (\i -> wrapOrT . f i) s
      in if visited
         then fu
         else itraverseOf_ b f s
infixl 3 `ifailing` -- Same as (<|>)
{-# INLINE ifailing #-}

----------------------------------------
-- Special folds

-- | Retrieve the first entry of an 'IxFold' along with its index.
--
-- >>> iheadOf ifolded [1..10]
-- Just (0,1)
iheadOf
  ::( Is k A_Fold, is `HasSingleIndex` i)
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
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
  :: (Is k A_Fold, is `HasSingleIndex` i)
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
 :: (Is k A_Fold, is `HasSingleIndex` i)
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
  :: (Is k A_Fold, Monad m, is `HasSingleIndex` i)
  => Optic' k is s a -> (i -> a -> m Bool) -> s -> m (Maybe (i, a))
ifindMOf o f = ifoldrOf o
  (\i a y -> f i a >>= \r -> if r then pure (Just (i, a)) else y)
  (pure Nothing)
{-# INLINE ifindMOf #-}
