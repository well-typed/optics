-- |
-- Module: Optics.Traversal1
-- Description: Lifts an effectful operation on elements to act on structures.
--
-- TBW: Traversal1 is to Traversal is like Semigroup to Monoid. 
--
module Optics.Traversal1
  (
  -- * Formation
    Traversal1
  , Traversal1'

  -- * Introduction
  , traversal1VL

  -- * Elimination
  , traverse1Of

  -- * Computation
  -- |
  --
  -- @
  -- 'traverseOf' ('traversalVL' f) ≡ f
  -- @

  -- * Well-formedness
  -- |
  --
  -- @
  -- 'traverseOf' o 'pure' ≡ 'pure'
  -- 'fmap' ('traverseOf' o f) . 'traverseOf' o g ≡ 'Data.Functor.Compose.getCompose' . 'traverseOf' o ('Data.Functor.Compose.Compose' . 'fmap' f . g)
  -- @

  -- * Additional introduction forms
  , traversed1

  -- * Additional elimination forms
  , for1Of
  , sequence1Of

{-
  , transposeOf
  , mapAccumROf
  , mapAccumLOf
  , scanr1Of
  , scanl1Of
  , failover
  , failover'

    -- * Combinators
  , backwards
  , partsOf
-}

  -- * Subtyping
  , A_Traversal1
  -- | <<diagrams/Traversal1.png Traversal1 in the optics hierarchy>>

  -- * van Laarhoven encoding
  -- | The van Laarhoven representation of a 'Traversal1' directly expresses how
  -- it lifts an effectful operation @A -> F B@ on elements to act on structures
  -- @S -> F T@.  Thus 'traverseOf' converts a 'Traversal1' to a 'Traversal1VL'.
  , Traversal1VL
  , Traversal1VL'
  )
  where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad.Trans.State
import Data.Functor.Identity

import Data.Profunctor.Indexed

import Optics.Fold
import Optics.Internal.Optic
import Optics.Internal.Utils
import Optics.Lens
import Optics.ReadOnly

import Data.List.NonEmpty (NonEmpty (..))

-- | Type synonym for a type-modifying traversal.
type Traversal1 s t a b = Optic A_Traversal1 NoIx s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal1' s a = Optic' A_Traversal1 NoIx s a

-- | Type synonym for a type-modifying van Laarhoven traversal.
type Traversal1VL s t a b = forall f. Apply f => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven traversal.
type Traversal1VL' s a = Traversal1VL s s a a

-- | Build a traversal from the van Laarhoven representation.
--
-- @
-- 'traversalVL' '.' 'traverseOf' ≡ 'id'
-- 'traverseOf' '.' 'traversalVL' ≡ 'id'
-- @
traversal1VL :: Traversal1VL s t a b -> Traversal1 s t a b
traversal1VL t = Optic (wander1 t)
{-# INLINE traversal1VL #-}

-- | Map each element of a structure targeted by a 'Traversal1', evaluate these
-- actions from left to right, and collect the results.
traverse1Of
  :: (Is k A_Traversal1, Apply f)
  => Optic k is s t a b
  -> (a -> f b) -> s -> f t
traverse1Of o = \f -> runStar1 $ getOptic (castOptic @A_Traversal1 o) (Star1 f)
{-# INLINE traverse1Of #-}

-- | A version of 'traverseOf' with the arguments flipped.
for1Of
  :: (Is k A_Traversal1, Apply f)
  => Optic k is s t a b
  -> s -> (a -> f b) -> f t
for1Of = flip . traverse1Of
{-# INLINE for1Of #-}

-- | Evaluate each action in the structure from left to right, and collect the
-- results.
--
-- >>> sequenceOf each ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
--
-- @
-- 'sequence' ≡ 'sequenceOf' 'traversed' ≡ 'traverse' 'id'
-- 'sequenceOf' o ≡ 'traverseOf' o 'id'
-- @
sequence1Of
  :: (Is k A_Traversal1, Apply f)
  => Optic k is s t (f b) b
  -> s -> f t
sequence1Of o = traverse1Of o id
{-# INLINE sequence1Of #-}

{- TODO
 
-- | This generalizes 'Data.List.transpose' to an arbitrary 'Traversal1'.
--
-- Note: 'Data.List.transpose' handles ragged inputs more intelligently, but for
-- non-ragged inputs:
--
-- >>> transposeOf traversed [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
--
-- @
-- 'Data.List.transpose' ≡ 'transposeOf' 'traverse'
-- @
transposeOf
  :: Is k A_Traversal1
  => Optic k is s t [a] a
  -> s -> [t]
transposeOf o = getZipList #. traverseOf o ZipList
{-# INLINE transposeOf #-}

-- | This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal1'.
--
-- @
-- 'Data.Traversable.mapAccumL' ≡ 'mapAccumLOf' 'traverse'
-- @
--
-- 'mapAccumLOf' accumulates 'State' from left to right.
mapAccumLOf
  :: Is k A_Traversal1
  => Optic k is s t a b
  -> (acc -> a -> (b, acc)) -> acc -> s -> (t, acc)
mapAccumLOf o = \f acc0 s ->
  let g a = state $ \acc -> f acc a
  in runState (traverseOf o g s) acc0

{-# INLINE mapAccumLOf #-}

-- | This generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal1'.
--
-- @
-- 'Data.Traversable.mapAccumR' ≡ 'mapAccumROf' 'traversed'
-- @
--
-- 'mapAccumROf' accumulates 'State' from right to left.
mapAccumROf
  :: Is k A_Traversal1
  => Optic k is s t a b
  -> (acc -> a -> (b, acc)) -> acc -> s -> (t, acc)
mapAccumROf = mapAccumLOf . backwards
{-# INLINE mapAccumROf #-}

-- | This permits the use of 'scanl1' over an arbitrary 'Traversal1'.
--
-- @
-- 'scanl1' ≡ 'scanl1Of' 'traversed'
-- @
scanl1Of
  :: Is k A_Traversal1
  => Optic k is s t a a
  -> (a -> a -> a) -> s -> t
scanl1Of o = \f ->
  let step Nothing a  = (a, Just a)
      step (Just s) a = let r = f s a in (r, Just r)
  in fst . mapAccumLOf o step Nothing
{-# INLINE scanl1Of #-}

-- | This permits the use of 'scanr1' over an arbitrary 'Traversal1'.
--
-- @
-- 'scanr1' ≡ 'scanr1Of' 'traversed'
-- @
scanr1Of
  :: Is k A_Traversal1
  => Optic k is s t a a
  -> (a -> a -> a) -> s -> t
scanr1Of o = \f ->
  let step Nothing a  = (a, Just a)
      step (Just s) a = let r = f a s in (r, Just r)
  in fst . mapAccumROf o step Nothing
{-# INLINE scanr1Of #-}

-- | Try to map a function over this 'Traversal1', returning Nothing if the
-- traversal has no targets.
--
-- >>> failover (element 3) (*2) [1,2]
-- Nothing
--
-- >>> failover _Left (*2) (Right 4)
-- Nothing
--
-- >>> failover _Right (*2) (Right 4)
-- Just (Right 8)
--
failover
  :: Is k A_Traversal1
  => Optic k is s t a b
  -> (a -> b) -> s -> Maybe t
failover o = \f s ->
  let OrT visited t = traverseOf o (wrapOrT . Identity #. f) s
  in if visited
     then Just (runIdentity t)
     else Nothing
{-# INLINE failover #-}

-- | Version of 'failover' strict in the application of @f@.
failover'
  :: Is k A_Traversal1
  => Optic k is s t a b
  -> (a -> b) -> s -> Maybe t
failover' o = \f s ->
  let OrT visited t = traverseOf o (wrapOrT . wrapIdentity' . f) s
  in if visited
     then Just (unwrapIdentity' t)
     else Nothing
{-# INLINE failover' #-}

-}

----------------------------------------
-- Traversal1s

-- | Construct a 'Traversal1' via the 'Traversable' class.
--
-- @
-- 'traverseOf' 'traversed' = 'traverse'
-- @
--
traversed1 :: Traversable1 t => Traversal1 (t a) (t b) a b
traversed1 = Optic (wander1 traverse1)
{-# INLINE traversed1 #-}

-- TODO: move to own module
-- TODO: also from semigroupoids
class Foldable f => Foldable1 f where
  foldMap1 :: Semigroup m => (a -> m) -> f a -> m
  foldMap1 f = foldMap1 f . toNonEmpty

  toNonEmpty :: f a -> NonEmpty a 
  toNonEmpty = foldMap1 (:|[]) -- TODO: use dlistnonempty?

  {-# MINIMAL foldMap1 | toNonEmpty #-}

class (Foldable1 t, Traversable t) => Traversable1 t where
  traverse1 :: Apply f => (a -> f b) -> t a -> f (t b)

  -- TODO: more instances

instance Foldable1 Identity where
  foldMap1 f (Identity x) = f x
  toNonEmpty (Identity x) = x :| []


instance Traversable1 Identity where
  traverse1 f = fmap Identity . f . runIdentity

instance Foldable1 NonEmpty where
  foldMap1 f (x :| [])     = f x
  foldMap1 f (x :| y : ys) = f x <> foldMap1 f (y :| ys)

  toNonEmpty = id

instance Traversable1 NonEmpty where
  traverse1 f (a :| []) = (:|[]) <$> f a
  traverse1 f (a :| (b: bs)) = (\a' (b':| bs') -> a' :| b': bs') <$> f a <.> traverse1 f (b :| bs)

{- TODO:

----------------------------------------
-- Traversal1 combinators

-- | This allows you to 'traverse' the elements of a traversal in the opposite
-- order.
backwards
  :: Is k A_Traversal1
  => Optic k is s t a b
  -> Traversal1 s t a b
backwards o = traversalVL $ \f -> forwards #. traverseOf o (Backwards #. f)
{-# INLINE backwards #-}

-- | 'partsOf' turns a 'Traversal1' into a 'Lens'.
--
-- /Note:/ You should really try to maintain the invariant of the number of
-- children in the list.
--
-- >>> ('a','b','c') & partsOf each .~ ['x','y','z']
-- ('x','y','z')
--
-- Any extras will be lost. If you do not supply enough, then the remainder will
-- come from the original structure.
--
-- >>> ('a','b','c') & partsOf each .~ ['w','x','y','z']
-- ('w','x','y')
--
-- >>> ('a','b','c') & partsOf each .~ ['x','y']
-- ('x','y','c')
--
-- >>> ('b', 'a', 'd', 'c') & partsOf each %~ sort
-- ('a','b','c','d')
--
-- So technically, this is only a 'Lens' if you do not change the number of
-- results it returns.
partsOf
  :: forall k is s t a. Is k A_Traversal1
  => Optic k is s t a a
  -> Lens s t [a] [a]
partsOf o = lensVL $ \f s -> evalState (traverseOf o update s)
  <$> f (toListOf (getting $ castOptic @A_Traversal1 o) s)
  where
    update a = get >>= \case
      a' : as' -> put as' >> pure a'
      []       ->            pure a
{-# INLINE partsOf #-}

-}

-- $setup
-- >>> import Data.List
-- >>> import Optics.Core
