-- |
-- Module: Optics.Fold
-- Description: Extracts elements from a container.
--
-- A @'Fold' S A@ has the ability to extract some number of elements of type @A@
-- from a container of type @S@.  For example, 'toListOf' can be used to obtain
-- the contained elements as a list. Unlike a 'Optics.Traversal.Traversal',
-- there is no way to set or update elements.
--
-- This can be seen as a generalisation of 'traverse_', where the type @S@ does
-- not need to be a type constructor with @A@ as the last parameter.
--
-- A close relative is the 'Optics.AffineFold.AffineFold', which is a 'Fold'
-- that contains at most one element.
--
module Optics.Fold
  (
  -- * Formation
    Fold

  -- * Introduction
  , foldVL

  -- * Elimination
  , foldOf
  , foldMapOf
  , foldrOf
  , foldlOf'
  , toListOf
  , sequenceOf_
  , traverseOf_
  , forOf_

  -- * Computation
  --
  -- |
  --
  -- @
  -- 'traverseOf_' ('foldVL' f) ≡ f
  -- @

  -- * Additional introduction forms
  , folded
  , folding
  , foldring
  , unfolded

  -- * Additional elimination forms
  -- | See also 'Data.Set.Optics.setOf', which constructs a 'Data.Set.Set' from a 'Fold'.
  , has
  , hasn't
  , headOf
  , lastOf
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
  , lengthOf
  , maximumOf
  , minimumOf
  , maximumByOf
  , minimumByOf
  , findOf
  , findMOf
  , lookupOf
  , universeOf
  , cosmosOf
  , paraOf

  -- * Combinators
  , pre
  , backwards_

  -- * Monoid structures #monoids#
  -- | 'Fold' admits (at least) two monoid structures:
  --
  -- * 'summing' concatenates results from both folds.
  --
  -- * 'failing' returns results from the second fold only if the first returns
  --   no results.
  --
  -- In both cases, the identity element of the monoid is
  -- `Optics.IxAffineTraversal.ignored`, which returns no results.
  --
  -- There is no 'Semigroup' or 'Monoid' instance for 'Fold', because there is
  -- not a unique choice of monoid to use, and the ('<>') operator could not be
  -- used to combine optics of different kinds.  When porting code from @lens@
  -- that uses '<>' to combine folds, use 'summing' instead.
  , summing
  , failing

  -- * Subtyping
  , A_Fold
  -- | <<diagrams/Fold.png Fold in the optics hierarchy>>
  )
  where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Monoid

import Data.Profunctor.Indexed

import Optics.AffineFold
import Optics.Internal.Bi
import Optics.Internal.Fold
import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Type synonym for a fold.
type Fold s a = Optic' A_Fold NoIx s a

-- | Construct a 'Fold' from a 'traverse_' like function.
--
-- /Note:/ for lifting a 'Data.Traversable.traverse' like function see
-- 'Optics.Traversal.traversalVL'.
--
-- @
-- 'foldVL' '.' 'traverseOf_' ≡ 'id'
-- 'traverseOf_' '.' 'foldVL' ≡ 'id'
-- @
foldVL
  :: (forall f. Applicative f => (a -> f b) -> s -> f ())
  -> Fold s a
foldVL f = Optic (foldVL__ f)
{-# INLINE foldVL #-}

-- | Combine the results of a fold using a monoid.
foldOf :: (Is k A_Fold, Monoid a) => Optic' k is s a -> s -> a
foldOf o = foldMapOf o id
{-# INLINE foldOf #-}

-- | Fold via embedding into a monoid.
foldMapOf :: (Is k A_Fold, Monoid m) => Optic' k is s a -> (a -> m) -> s -> m
foldMapOf o = runForget #. getOptic (castOptic @A_Fold o) .# Forget
{-# INLINE foldMapOf #-}

-- | Fold right-associatively.
foldrOf :: Is k A_Fold => Optic' k is s a -> (a -> r -> r) -> r -> s -> r
foldrOf o = \arr r s -> (\e -> appEndo e r) $ foldMapOf o (Endo #. arr) s
{-# INLINE foldrOf #-}

-- | Fold left-associatively, and strictly.
foldlOf' :: Is k A_Fold => Optic' k is s a -> (r -> a -> r) -> r -> s -> r
foldlOf' o = \rar r0 s -> foldrOf o (\a rr r -> rr $! rar r a) id s r0
{-# INLINE foldlOf' #-}

-- | Fold to a list.
--
-- >>> toListOf (_1 % folded % _Right) ([Right 'h', Left 5, Right 'i'], "bye")
-- "hi"
toListOf :: Is k A_Fold => Optic' k is s a -> s -> [a]
toListOf o = foldrOf o (:) []
{-# INLINE toListOf #-}

----------------------------------------

-- | Traverse over all of the targets of a 'Fold', computing an
-- 'Applicative'-based answer, but unlike 'Optics.Traversal.traverseOf' do not
-- construct a new structure. 'traverseOf_' generalizes
-- 'Data.Foldable.traverse_' to work over any 'Fold'.
--
-- >>> traverseOf_ each putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.traverse_' ≡ 'traverseOf_' 'folded'
-- @
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

-- | Evaluate each action in a structure observed by a 'Fold' from left to
-- right, ignoring the results.
--
-- @
-- 'sequenceA_' ≡ 'sequenceOf_' 'folded'
-- @
--
-- >>> sequenceOf_ each (putStrLn "hello",putStrLn "world")
-- hello
-- world
sequenceOf_
  :: (Is k A_Fold, Applicative f)
  => Optic' k is s (f a)
  -> s -> f ()
sequenceOf_ o = runTraversed . foldMapOf o Traversed
{-# INLINE sequenceOf_ #-}

----------------------------------------

-- | Fold via the 'Foldable' class.
folded :: Foldable f => Fold (f a) a
folded = Optic folded__
{-# INLINE folded #-}

-- | Obtain a 'Fold' by lifting an operation that returns a 'Foldable' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a
-- 'Fold'.
--
-- >>> toListOf (folding tail) [1,2,3,4]
-- [2,3,4]
folding :: Foldable f => (s -> f a) -> Fold s a
folding f = Optic (contrafirst f . foldVL__ traverse_)
{-# INLINE folding #-}

-- | Obtain a 'Fold' by lifting 'foldr' like function.
--
-- >>> toListOf (foldring foldr) [1,2,3,4]
-- [1,2,3,4]
foldring
  :: (forall f. Applicative f => (a -> f u -> f u) -> f v -> s -> f w)
  -> Fold s a
foldring fr = Optic (foldring__ fr)
{-# INLINE foldring #-}

-- | Build a 'Fold' that unfolds its values from a seed.
--
-- @
-- 'Prelude.unfoldr' ≡ 'toListOf' '.' 'unfolded'
-- @
--
-- >>> toListOf (unfolded $ \b -> if b == 0 then Nothing else Just (b, b - 1)) 10
-- [10,9,8,7,6,5,4,3,2,1]
unfolded :: (s -> Maybe (a, s)) -> Fold s a
unfolded step = foldVL $ \f -> fix $ \loop b ->
  case step b of
    Just (a, b') -> f a *> loop b'
    Nothing      -> pure ()
{-# INLINE unfolded #-}

-- | Convert a fold to an 'AffineFold' that visits the first element of the
-- original fold.
--
-- For the traversal version see 'Optics.Traversal.singular'.
pre :: Is k A_Fold => Optic' k is s a -> AffineFold s a
pre = afolding . headOf
{-# INLINE pre #-}

-- | This allows you to traverse the elements of a 'Fold' in the opposite order.
backwards_
  :: Is k A_Fold
  => Optic' k is s a
  -> Fold s a
backwards_ o = foldVL $ \f -> forwards #. traverseOf_ o (Backwards #. f)
{-# INLINE backwards_ #-}

-- | Return entries of the first 'Fold', then the second one.
--
-- >>> toListOf (_1 % ix 0 `summing` _2 % ix 1) ([1,2], [4,7,1])
-- [1,7]
--
-- For the traversal version see 'Optics.Traversal.adjoin'.
summing
  :: (Is k A_Fold, Is l A_Fold)
  => Optic' k is s a
  -> Optic' l js s a
  -> Fold s a
summing a b = foldVL $ \f s -> traverseOf_ a f s *> traverseOf_ b f s
infixr 6 `summing` -- Same as (<>)
{-# INLINE summing #-}

-- | Try the first 'Fold'. If it returns no entries, try the second one.
--
-- >>> toListOf (ix 1 `failing` ix 0) [4,7]
-- [7]
-- >>> toListOf (ix 1 `failing` ix 0) [4]
-- [4]
--
failing
  :: (Is k A_Fold, Is l A_Fold)
  => Optic' k is s a
  -> Optic' l js s a
  -> Fold s a
failing a b = foldVL $ \f s ->
  let OrT visited fu = traverseOf_ a (wrapOrT . f) s
  in if visited
     then fu
     else traverseOf_ b f s
infixl 3 `failing` -- Same as (<|>)
{-# INLINE failing #-}

----------------------------------------
-- Special folds

-- | Check to see if this optic matches 1 or more entries.
--
-- >>> has _Left (Left 12)
-- True
--
-- >>> has _Right (Left 12)
-- False
--
-- This will always return 'True' for a 'Optics.Lens.Lens' or
-- 'Optics.Getter.Getter'.
--
-- >>> has _1 ("hello","world")
-- True
has :: Is k A_Fold => Optic' k is s a -> s -> Bool
has o = getAny #. foldMapOf o (\_ -> Any True)
{-# INLINE has #-}

-- | Check to see if this 'Fold' or 'Optics.Traversal.Traversal' has
-- no matches.
--
-- >>> hasn't _Left (Right 12)
-- True
--
-- >>> hasn't _Left (Left 12)
-- False
hasn't :: Is k A_Fold => Optic' k is s a -> s -> Bool
hasn't o = getAll #. foldMapOf o (\_ -> All False)
{-# INLINE hasn't #-}

-- | Retrieve the first entry of a 'Fold'.
--
-- >>> headOf folded [1..10]
-- Just 1
--
-- >>> headOf each (1,2)
-- Just 1
headOf :: Is k A_Fold => Optic' k is s a -> s -> Maybe a
headOf o = getLeftmost . foldMapOf o LLeaf
{-# INLINE headOf #-}

-- | Retrieve the last entry of a 'Fold'.
--
-- >>> lastOf folded [1..10]
-- Just 10
--
-- >>> lastOf each (1,2)
-- Just 2
lastOf :: Is k A_Fold => Optic' k is s a -> s -> Maybe a
lastOf o = getRightmost . foldMapOf o RLeaf
{-# INLINE lastOf #-}

-- | Returns 'True' if every target of a 'Fold' is 'True'.
--
-- >>> andOf each (True, False)
-- False
-- >>> andOf each (True, True)
-- True
--
-- @
-- 'Data.Foldable.and' ≡ 'andOf' 'folded'
-- @
andOf :: Is k A_Fold => Optic' k is s Bool -> s -> Bool
andOf o = getAll #. foldMapOf o All
{-# INLINE andOf #-}

-- | Returns 'True' if any target of a 'Fold' is 'True'.
--
-- >>> orOf each (True, False)
-- True
-- >>> orOf each (False, False)
-- False
--
-- @
-- 'Data.Foldable.or' ≡ 'orOf' 'folded'
-- @
orOf :: Is k A_Fold => Optic' k is s Bool -> s -> Bool
orOf o = getAny #. foldMapOf o Any
{-# INLINE orOf #-}

-- | Returns 'True' if any target of a 'Fold' satisfies a predicate.
--
-- >>> anyOf each (=='x') ('x','y')
-- True
anyOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
anyOf o = \f -> getAny #. foldMapOf o (Any #. f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold' satisfies a predicate.
--
-- >>> allOf each (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @
-- 'Data.Foldable.all' ≡ 'allOf' 'folded'
-- @
allOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
allOf o = \f -> getAll #. foldMapOf o (All #. f)
{-# INLINE allOf #-}

-- | Returns 'True' only if no targets of a 'Fold' satisfy a predicate.
--
-- >>> noneOf each (not . isn't _Nothing) (Just 3, Just 4, Just 5)
-- True
-- >>> noneOf (folded % folded) (<10) [[13,99,20],[3,71,42]]
-- False
noneOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Bool
noneOf o = \f -> not . anyOf o f
{-# INLINE noneOf #-}

-- | Calculate the 'Product' of every number targeted by a 'Fold'.
--
-- >>> productOf each (4,5)
-- 20
-- >>> productOf folded [1,2,3,4,5]
-- 120
--
-- @
-- 'Data.Foldable.product' ≡ 'productOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you want a lazier
-- version use @\\o -> 'getProduct' '.' 'foldMapOf' o 'Product'@.
productOf :: (Is k A_Fold, Num a) => Optic' k is s a -> s -> a
productOf o = foldlOf' o (*) 1
{-# INLINE productOf #-}

-- | Calculate the 'Sum' of every number targeted by a 'Fold'.
--
-- >>> sumOf each (5,6)
-- 11
-- >>> sumOf folded [1,2,3,4]
-- 10
-- >>> sumOf (folded % each) [(1,2),(3,4)]
-- 10
--
-- @
-- 'Data.Foldable.sum' ≡ 'sumOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you want a lazier
-- version use @\\o -> 'getSum' '.' 'foldMapOf' o 'Sum'@
sumOf :: (Is k A_Fold, Num a) => Optic' k is s a -> s -> a
sumOf o = foldlOf' o (+) 0
{-# INLINE sumOf #-}

-- | The sum of a collection of actions.
--
-- >>> asumOf each ("hello","world")
-- "helloworld"
--
-- >>> asumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'asum' ≡ 'asumOf' 'folded'
-- @
asumOf :: (Is k A_Fold, Alternative f) => Optic' k is s (f a) -> s -> f a
asumOf o = foldrOf o (<|>) empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions.
--
-- >>> msumOf each ("hello","world")
-- "helloworld"
--
-- >>> msumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'msum' ≡ 'msumOf' 'folded'
-- @
msumOf :: (Is k A_Fold, MonadPlus m) => Optic' k is s (m a) -> s -> m a
msumOf o = foldrOf o mplus mzero
{-# INLINE msumOf #-}

-- | Does the element occur anywhere within a given 'Fold' of the structure?
--
-- >>> elemOf each "hello" ("hello","world")
-- True
--
-- @
-- 'elem' ≡ 'elemOf' 'folded'
-- @
elemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
elemOf o = anyOf o . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- >>> notElemOf each 'd' ('a','b','c')
-- True
--
-- >>> notElemOf each 'a' ('a','b','c')
-- False
--
-- @
-- 'notElem' ≡ 'notElemOf' 'folded'
-- @
notElemOf :: (Is k A_Fold, Eq a) => Optic' k is s a -> a -> s -> Bool
notElemOf o = allOf o . (/=)
{-# INLINE notElemOf #-}

-- | Calculate the number of targets there are for a 'Fold' in a given
-- container.
--
-- /Note:/ This can be rather inefficient for large containers and just like
-- 'length', this will not terminate for infinite folds.
--
-- @
-- 'length' ≡ 'lengthOf' 'folded'
-- @
--
-- >>> lengthOf _1 ("hello",())
-- 1
--
-- >>> lengthOf folded [1..10]
-- 10
--
-- >>> lengthOf (folded % folded) [[1,2],[3,4],[5,6]]
-- 6
lengthOf :: Is k A_Fold => Optic' k is s a -> s -> Int
lengthOf o = foldlOf' o (\ n _ -> 1 + n) 0
{-# INLINE lengthOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold' safely.
--
-- Note: 'maximumOf' on a valid 'Optics.Iso.Iso', 'Optics.Lens.Lens'
-- or 'Optics.Getter.Getter' will always return 'Just' a value.
--
-- >>> maximumOf folded [1..10]
-- Just 10
--
-- >>> maximumOf folded []
-- Nothing
--
-- >>> maximumOf (folded % filtered even) [1,4,3,6,7,9,2]
-- Just 6
--
-- @
-- 'maximum' ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than
-- strictly necessary.  @\\o -> 'Data.Semigroup.getMax' . 'foldMapOf' o 'Data.Semigroup.Max'@ has lazier
-- semantics but could leak memory.
maximumOf :: (Is k A_Fold, Ord a) => Optic' k is s a -> s -> Maybe a
maximumOf o = foldlOf' o mf Nothing where
  mf Nothing y  = Just $! y
  mf (Just x) y = Just $! max x y
{-# INLINE maximumOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold' safely.
--
-- Note: 'minimumOf' on a valid 'Optics.Iso.Iso', 'Optics.Lens.Lens'
-- or 'Optics.Getter.Getter' will always return 'Just' a value.
--
-- >>> minimumOf folded [1..10]
-- Just 1
--
-- >>> minimumOf folded []
-- Nothing
--
-- >>> minimumOf (folded % filtered even) [1,4,3,6,7,9,2]
-- Just 2
--
-- @
-- 'minimum' ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than
-- strictly necessary.  @\\o -> 'Data.Semigroup.getMin' . 'foldMapOf' o 'Data.Semigroup.Min'@ has lazier
-- semantics but could leak memory.
minimumOf :: (Is k A_Fold, Ord a) => Optic' k is s a -> s -> Maybe a
minimumOf o = foldlOf' o mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! min x y
{-# INLINE minimumOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold' according to a
-- user supplied 'Ordering'.
--
-- >>> maximumByOf folded (compare `on` length) ["mustard","relish","ham"]
-- Just "mustard"
--
-- In the interest of efficiency, This operation has semantics more strict than
-- strictly necessary.
--
-- @
-- 'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumByOf' 'folded' cmp
-- @
maximumByOf :: Is k A_Fold => Optic' k is s a -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf o = \cmp ->
  let mf Nothing y  = Just $! y
      mf (Just x) y = Just $! if cmp x y == GT then x else y
  in foldlOf' o mf Nothing
{-# INLINE maximumByOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold' according to a
-- user supplied 'Ordering'.
--
-- In the interest of efficiency, This operation has semantics more strict than
-- strictly necessary.
--
-- >>> minimumByOf folded (compare `on` length) ["mustard","relish","ham"]
-- Just "ham"
--
-- @
-- 'minimumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumByOf' 'folded' cmp
-- @
minimumByOf :: Is k A_Fold => Optic' k is s a -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf o = \cmp ->
  let mf Nothing y  = Just $! y
      mf (Just x) y = Just $! if cmp x y == GT then y else x
  in foldlOf' o mf Nothing
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Fold', a predicate and a structure and
-- returns the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
--
-- >>> findOf each even (1,3,4,6)
-- Just 4
--
-- >>> findOf folded even [1,3,5,7]
-- Nothing
--
-- @
-- 'Data.Foldable.find' ≡ 'findOf' 'folded'
-- @
findOf :: Is k A_Fold => Optic' k is s a -> (a -> Bool) -> s -> Maybe a
findOf o = \f -> foldrOf o (\a y -> if f a then Just a else y) Nothing
{-# INLINE findOf #-}

-- | The 'findMOf' function takes a 'Fold', a monadic predicate and a structure
-- and returns in the monad the leftmost element of the structure matching the
-- predicate, or 'Nothing' if there is no such element.
--
-- >>> findMOf each (\x -> print ("Checking " ++ show x) >> return (even x)) (1,3,4,6)
-- "Checking 1"
-- "Checking 3"
-- "Checking 4"
-- Just 4
--
-- >>> findMOf each (\x -> print ("Checking " ++ show x) >> return (even x)) (1,3,5,7)
-- "Checking 1"
-- "Checking 3"
-- "Checking 5"
-- "Checking 7"
-- Nothing
--
-- @
-- 'findMOf' 'folded' :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m (Maybe a)
-- @
findMOf :: (Is k A_Fold, Monad m) => Optic' k is s a -> (a -> m Bool) -> s -> m (Maybe a)
findMOf o = \f -> foldrOf o
  (\a y -> f a >>= \r -> if r then pure (Just a) else y)
  (pure Nothing)
{-# INLINE findMOf #-}

-- | The 'lookupOf' function takes a 'Fold', a key, and a structure containing
-- key/value pairs.  It returns the first value corresponding to the given
-- key. This function generalizes 'lookup' to work on an arbitrary 'Fold'
-- instead of lists.
--
-- >>> lookupOf folded 4 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'b'
--
-- >>> lookupOf folded 2 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'a'
lookupOf :: (Is k A_Fold, Eq a) => Optic' k is s (a, v) -> a -> s -> Maybe v
lookupOf o a = foldrOf o (\(a', v) next -> if a == a' then Just v else next) Nothing
{-# INLINE lookupOf #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of
-- the transitive descendants of a node, including itself.
--
-- @since 0.4.1
universeOf :: Is k A_Fold => Optic' k is a a -> a -> [a]
universeOf o = (`appEndo` []) . go
  where
    go a = Endo (a :) <> foldMapOf o go a
{-# INLINE universeOf #-}

-- | Given a 'Fold' that knows how to locate immediate children, fold all of the
-- transitive descendants of a node, including itself.
--
-- @since 0.4.1
cosmosOf :: forall k is a. Is k A_Fold => Optic' k is a a -> Fold a a
cosmosOf o = foldVL go
  where
    go :: Applicative f => (a -> f ()) -> a -> f ()
    go f a = f a *> traverseOf_ o (go f) a
{-# INLINE cosmosOf #-}

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @since 0.4.1
paraOf :: Is k A_Fold => Optic' k is a a -> (a -> [r] -> r) -> a -> r
paraOf o f = go
  where
    go a = f a (go <$> toListOf o a)
{-# INLINE paraOf #-}

-- $setup
-- >>> import Optics.Core
