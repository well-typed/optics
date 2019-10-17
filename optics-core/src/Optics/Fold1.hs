-- |
-- Module: Optics.Fold1
-- Description: Extracts elements from a non-empty container.
--
-- TBW. Non 
--
module Optics.Fold1
  (
  -- * Formation
    Fold1

  -- * Introduction
  , fold1VL

  -- * Elimination
  , fold1Of
  , foldMap1Of
  -- , foldr1Of
  -- , foldl1Of'

  , toNonEmptyOf

{-
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
  -- | See also 'Data.Set.Optics.setOf', which constructs a 'Data.Set.Set' from a 'Fold1'.
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

  -- * Combinators
  , pre
  , backwards_

  -- * Semigroup structure
  , summing
  , failing
-}

  -- * Subtyping
  , A_Fold1
  -- | <<diagrams/Fold1.png Fold1 in the optics hierarchy>>
  )
  where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad
import Data.Foldable
import Data.Function
import Data.Monoid
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.List.NonEmpty as NE

import Data.Profunctor.Indexed

import Optics.AffineFold
import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Type synonym for a fold.
type Fold1 s a = Optic' A_Fold1 NoIx s a

-- | Obtain a 'Fold1' by lifting 'traverse_' like function.
--
-- @
-- 'foldVL' '.' 'traverseOf_' ≡ 'id'
-- 'traverseOf_' '.' 'foldVL' ≡ 'id'
-- @
fold1VL
  :: (forall f. Apply f => (a -> f u) -> s -> f v)
  -> Fold1 s a
fold1VL f = Optic (rphantom . wander1 f . rphantom)
{-# INLINE fold1VL #-}

-- | Combine the results of a fold using a monoid.
fold1Of :: (Is k A_Fold1, Semigroup a) => Optic' k is s a -> s -> a
fold1Of o = foldMap1Of o id
{-# INLINE fold1Of #-}

-- | Fold1 via embedding into a monoid.
foldMap1Of :: (Is k A_Fold1, Semigroup m) => Optic' k is s a -> (a -> m) -> s -> m
foldMap1Of o = runForget #. getOptic (castOptic @A_Fold1 o) .# Forget
{-# INLINE foldMap1Of #-}
 
{-
-- | Fold1 right-associatively.
foldr1Of :: Is k A_Fold1 => Optic' k is s a -> (a -> a -> a) -> s -> a
foldr1Of o = \arr s -> (\e -> appEndo e r) $ foldMap1Of o (Endo #. arr) s
{-# INLINE foldr1Of #-}
-}

newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  (<>) = append
  {-# INLINE (<>) #-}

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = NEDL . (:|)

-- | /O(1)/. Append dlists
append :: NonEmptyDList a -> NonEmptyDList a -> NonEmptyDList a
append xs ys = NEDL (unNEDL xs . NE.toList . unNEDL ys)

-- | Convert a dlist to a non-empty list
toNonEmpty :: NonEmptyDList a -> NonEmpty a
toNonEmpty = ($[]) . unNEDL
{-# INLINE toNonEmpty #-}

{-
-- | Fold1 left-associatively, and strictly.
foldl1Of' :: Is k A_Fold1 => Optic' k is s a -> (r -> a -> r) -> r -> s -> r
foldl1Of' o = \rar r0 s -> foldr1Of o (\a rr r -> rr $! rar r a) id s r0
{-# INLINE foldl1Of' #-}
-}

-- | Fold1 to a list.
toNonEmptyOf :: Is k A_Fold1 => Optic' k is s a -> s -> NonEmpty a
toNonEmptyOf o = toNonEmpty . foldMap1Of o singleton
{-# INLINE toNonEmptyOf #-}

{-
----------------------------------------

-- | Traverse over all of the targets of a 'Fold1', computing an
-- 'Applicative'-based answer, but unlike 'Optics.Traversal.traverseOf' do not
-- construct a new structure. 'traverseOf_' generalizes
-- 'Data.Fold1able.traverse_' to work over any 'Fold1'.
--
-- >>> traverseOf_ each putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Fold1able.traverse_' ≡ 'traverseOf_' 'folded'
-- @
traverseOf_
  :: (Is k A_Fold1, Applicative f)
  => Optic' k is s a
  -> (a -> f r) -> s -> f ()
traverseOf_ o = \f -> runTraversed . foldMapOf o (Traversed #. f)
{-# INLINE traverseOf_ #-}

-- | A version of 'traverseOf_' with the arguments flipped.
forOf_
  :: (Is k A_Fold1, Applicative f)
  => Optic' k is s a
  -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in a structure observed by a 'Fold1' from left to
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
  :: (Is k A_Fold1, Applicative f)
  => Optic' k is s (f a)
  -> s -> f ()
sequenceOf_ o = runTraversed . foldMapOf o Traversed
{-# INLINE sequenceOf_ #-}

----------------------------------------

-- | Fold1 via the 'Fold1able' class.
folded :: Fold1able f => Fold1 (f a) a
folded = Optic folded__
{-# INLINE folded #-}

-- | Obtain a 'Fold1' by lifting an operation that returns a 'Fold1able' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a
-- 'Fold1'.
--
-- >>> toListOf (folding tail) [1,2,3,4]
-- [2,3,4]
folding :: Fold1able f => (s -> f a) -> Fold1 s a
folding f = Optic (contrafirst f . foldVL__ traverse_)
{-# INLINE folding #-}

-- | Obtain a 'Fold1' by lifting 'foldr' like function.
--
-- >>> toListOf (foldring foldr) [1,2,3,4]
-- [1,2,3,4]
foldring
  :: (forall f. Applicative f => (a -> f u -> f u) -> f v -> s -> f w)
  -> Fold1 s a
foldring fr = Optic (foldring__ fr)
{-# INLINE foldring #-}

-- | Build a 'Fold1' that unfolds its values from a seed.
--
-- @
-- 'Prelude.unfoldr' ≡ 'toListOf' '.' 'unfolded'
-- @
--
-- >>> toListOf (unfolded $ \b -> if b == 0 then Nothing else Just (b, b - 1)) 10
-- [10,9,8,7,6,5,4,3,2,1]
unfolded :: (s -> Maybe (a, s)) -> Fold1 s a
unfolded step = foldVL $ \f -> fix $ \loop b ->
  case step b of
    Just (a, b') -> f a *> loop b'
    Nothing      -> pure ()
{-# INLINE unfolded #-}

-- | Convert a fold to an 'AffineFold1' that visits the first element of the
-- original fold.
pre :: Is k A_Fold1 => Optic' k is s a -> AffineFold1 s a
pre = afolding . headOf
{-# INLINE pre #-}

-- | This allows you to traverse the elements of a 'Fold1' in the opposite order.
backwards_
  :: Is k A_Fold1
  => Optic' k is s a
  -> Fold1 s a
backwards_ o = foldVL $ \f -> forwards #. traverseOf_ o (Backwards #. f)
{-# INLINE backwards_ #-}

-- | Return entries of the first 'Fold1', then the second one.
--
-- >>> toListOf (_1 % ix 0 `summing` _2 % ix 1) ([1,2], [4,7,1])
-- [1,7]
--
summing
  :: (Is k A_Fold1, Is l A_Fold1)
  => Optic' k is s a
  -> Optic' l js s a
  -> Fold1 s a
summing a b = foldVL $ \f s -> traverseOf_ a f s *> traverseOf_ b f s
infixr 6 `summing` -- Same as (<>)
{-# INLINE summing #-}

-- | Try the first 'Fold1'. If it returns no entries, try the second one.
failing
  :: (Is k A_Fold1, Is l A_Fold1)
  => Optic' k is s a
  -> Optic' l js s a
  -> Fold1 s a
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
has :: Is k A_Fold1 => Optic' k is s a -> s -> Bool
has o = getAny #. foldMapOf o (\_ -> Any True)
{-# INLINE has #-}

-- | Check to see if this 'Fold1' or 'Optics.Traversal.Traversal' has
-- no matches.
--
-- >>> hasn't _Left (Right 12)
-- True
--
-- >>> hasn't _Left (Left 12)
-- False
hasn't :: Is k A_Fold1 => Optic' k is s a -> s -> Bool
hasn't o = getAll #. foldMapOf o (\_ -> All False)
{-# INLINE hasn't #-}

-- | Retrieve the first entry of a 'Fold1'.
--
-- >>> headOf folded [1..10]
-- Just 1
--
-- >>> headOf each (1,2)
-- Just 1
headOf :: Is k A_Fold1 => Optic' k is s a -> s -> Maybe a
headOf o = getLeftmost . foldMapOf o LLeaf
{-# INLINE headOf #-}

-- | Retrieve the last entry of a 'Fold1'.
--
-- >>> lastOf folded [1..10]
-- Just 10
--
-- >>> lastOf each (1,2)
-- Just 2
lastOf :: Is k A_Fold1 => Optic' k is s a -> s -> Maybe a
lastOf o = getRightmost . foldMapOf o RLeaf
{-# INLINE lastOf #-}

-- | Returns 'True' if every target of a 'Fold1' is 'True'.
--
-- >>> andOf each (True, False)
-- False
-- >>> andOf each (True, True)
-- True
--
-- @
-- 'Data.Fold1able.and' ≡ 'andOf' 'folded'
-- @
andOf :: Is k A_Fold1 => Optic' k is s Bool -> s -> Bool
andOf o = getAll #. foldMapOf o All
{-# INLINE andOf #-}

-- | Returns 'True' if any target of a 'Fold1' is 'True'.
--
-- >>> orOf each (True, False)
-- True
-- >>> orOf each (False, False)
-- False
--
-- @
-- 'Data.Fold1able.or' ≡ 'orOf' 'folded'
-- @
orOf :: Is k A_Fold1 => Optic' k is s Bool -> s -> Bool
orOf o = getAny #. foldMapOf o Any
{-# INLINE orOf #-}

-- | Returns 'True' if any target of a 'Fold1' satisfies a predicate.
--
-- >>> anyOf each (=='x') ('x','y')
-- True
anyOf :: Is k A_Fold1 => Optic' k is s a -> (a -> Bool) -> s -> Bool
anyOf o = \f -> getAny #. foldMapOf o (Any #. f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold1' satisfies a predicate.
--
-- >>> allOf each (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @
-- 'Data.Fold1able.all' ≡ 'allOf' 'folded'
-- @
allOf :: Is k A_Fold1 => Optic' k is s a -> (a -> Bool) -> s -> Bool
allOf o = \f -> getAll #. foldMapOf o (All #. f)
{-# INLINE allOf #-}

-- | Returns 'True' only if no targets of a 'Fold1' satisfy a predicate.
--
-- >>> noneOf each (not . isn't _Nothing) (Just 3, Just 4, Just 5)
-- True
-- >>> noneOf (folded % folded) (<10) [[13,99,20],[3,71,42]]
-- False
noneOf :: Is k A_Fold1 => Optic' k is s a -> (a -> Bool) -> s -> Bool
noneOf o = \f -> not . anyOf o f
{-# INLINE noneOf #-}

-- | Calculate the 'Product' of every number targeted by a 'Fold1'.
--
-- >>> productOf each (4,5)
-- 20
-- >>> productOf folded [1,2,3,4,5]
-- 120
--
-- @
-- 'Data.Fold1able.product' ≡ 'productOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you want a lazier
-- version use @\\o -> 'getProduct' '.' 'foldMapOf' o 'Product'@.
productOf :: (Is k A_Fold1, Num a) => Optic' k is s a -> s -> a
productOf o = foldlOf' o (*) 1
{-# INLINE productOf #-}

-- | Calculate the 'Sum' of every number targeted by a 'Fold1'.
--
-- >>> sumOf each (5,6)
-- 11
-- >>> sumOf folded [1,2,3,4]
-- 10
-- >>> sumOf (folded % each) [(1,2),(3,4)]
-- 10
--
-- @
-- 'Data.Fold1able.sum' ≡ 'sumOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you want a lazier
-- version use @\\o -> 'getSum' '.' 'foldMapOf' o 'Sum'@
sumOf :: (Is k A_Fold1, Num a) => Optic' k is s a -> s -> a
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
asumOf :: (Is k A_Fold1, Alternative f) => Optic' k is s (f a) -> s -> f a
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
msumOf :: (Is k A_Fold1, MonadPlus m) => Optic' k is s (m a) -> s -> m a
msumOf o = foldrOf o mplus mzero
{-# INLINE msumOf #-}

-- | Does the element occur anywhere within a given 'Fold1' of the structure?
--
-- >>> elemOf each "hello" ("hello","world")
-- True
--
-- @
-- 'elem' ≡ 'elemOf' 'folded'
-- @
elemOf :: (Is k A_Fold1, Eq a) => Optic' k is s a -> a -> s -> Bool
elemOf o = anyOf o . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold1' of the structure?
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
notElemOf :: (Is k A_Fold1, Eq a) => Optic' k is s a -> a -> s -> Bool
notElemOf o = allOf o . (/=)
{-# INLINE notElemOf #-}

-- | Calculate the number of targets there are for a 'Fold1' in a given
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
lengthOf :: Is k A_Fold1 => Optic' k is s a -> s -> Int
lengthOf o = foldlOf' o (\ n _ -> 1 + n) 0
{-# INLINE lengthOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold1' safely.
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
maximumOf :: (Is k A_Fold1, Ord a) => Optic' k is s a -> s -> Maybe a
maximumOf o = foldlOf' o mf Nothing where
  mf Nothing y  = Just $! y
  mf (Just x) y = Just $! max x y
{-# INLINE maximumOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold1' safely.
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
minimumOf :: (Is k A_Fold1, Ord a) => Optic' k is s a -> s -> Maybe a
minimumOf o = foldlOf' o mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! min x y
{-# INLINE minimumOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold1' according to a
-- user supplied 'Ordering'.
--
-- >>> maximumByOf folded (compare `on` length) ["mustard","relish","ham"]
-- Just "mustard"
--
-- In the interest of efficiency, This operation has semantics more strict than
-- strictly necessary.
--
-- @
-- 'Data.Fold1able.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumByOf' 'folded' cmp
-- @
maximumByOf :: Is k A_Fold1 => Optic' k is s a -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf o = \cmp ->
  let mf Nothing y  = Just $! y
      mf (Just x) y = Just $! if cmp x y == GT then x else y
  in foldlOf' o mf Nothing
{-# INLINE maximumByOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold1' according to a
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
minimumByOf :: Is k A_Fold1 => Optic' k is s a -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf o = \cmp ->
  let mf Nothing y  = Just $! y
      mf (Just x) y = Just $! if cmp x y == GT then y else x
  in foldlOf' o mf Nothing
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Fold1', a predicate and a structure and
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
-- 'Data.Fold1able.find' ≡ 'findOf' 'folded'
-- @
findOf :: Is k A_Fold1 => Optic' k is s a -> (a -> Bool) -> s -> Maybe a
findOf o = \f -> foldrOf o (\a y -> if f a then Just a else y) Nothing
{-# INLINE findOf #-}

-- | The 'findMOf' function takes a 'Fold1', a monadic predicate and a structure
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
-- 'findMOf' 'folded' :: (Monad m, Fold1able f) => (a -> m Bool) -> f a -> m (Maybe a)
-- @
findMOf :: (Is k A_Fold1, Monad m) => Optic' k is s a -> (a -> m Bool) -> s -> m (Maybe a)
findMOf o = \f -> foldrOf o
  (\a y -> f a >>= \r -> if r then pure (Just a) else y)
  (pure Nothing)
{-# INLINE findMOf #-}

-- | The 'lookupOf' function takes a 'Fold1', a key, and a structure containing
-- key/value pairs.  It returns the first value corresponding to the given
-- key. This function generalizes 'lookup' to work on an arbitrary 'Fold1'
-- instead of lists.
--
-- >>> lookupOf folded 4 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'b'
--
-- >>> lookupOf folded 2 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'a'
lookupOf :: (Is k A_Fold1, Eq a) => Optic' k is s (a, v) -> a -> s -> Maybe v
lookupOf o a = foldrOf o (\(a', v) next -> if a == a' then Just v else next) Nothing
{-# INLINE lookupOf #-}
-}

-- $setup
-- >>> import Optics.Core
