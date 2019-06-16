-- |
-- Module: Optics.Traversal
-- Description: Lifts an effectful operation on elements to act on structures.
--
-- A 'Traversal' lifts an effectful operation on elements to act on structures
-- containing those elements.
--
-- That is, given a function @op :: A -> F B@ where @F@ is
-- 'Applicative', a @'Traversal' S T A B@ can produce a function @S ->
-- F T@ that applies @op@ to all the @A@s contained in the @S@.
--
-- This can be seen as a generalisation of 'traverse', where the type
-- @S@ does not need to be a type constructor with @A@ as the last
-- parameter.
--
-- A 'Lens' is a 'Traversal' that acts on a single value.
--
-- A close relative is the 'Optics.AffineTraversal.AffineTraversal',
-- which is a 'Traversal' that acts on at most one value.
--
module Optics.Traversal
  (
  -- * Formation
    Traversal
  , Traversal'

  -- * Introduction
  , traversalVL

  -- * Elimination
  , traverseOf

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
  , traversed

  -- * Additional elimination forms
  , forOf
  , sequenceOf
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

  -- * Subtyping
  , A_Traversal

  -- * van Laarhoven encoding
  -- | The van Laarhoven representation of a 'Traversal' directly expresses how
  -- it lifts an effectful operation @A -> F B@ on elements to act on structures
  -- @S -> F T@.  Thus 'traverseOf' converts a 'Traversal' to a 'TraversalVL'.
  , TraversalVL
  , TraversalVL'

  -- * Re-exports
  , module Optics.Optic
  )
  where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad.Trans.State
import Data.Functor.Identity

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Traversal
import Optics.Internal.Utils
import Optics.Lens
import Optics.Fold
import Optics.Optic
import Optics.ReadOnly

-- | Type synonym for a type-modifying traversal.
type Traversal s t a b = Optic A_Traversal NoIx s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal' s a = Optic' A_Traversal NoIx s a

-- | Type synonym for a type-modifying van Laarhoven traversal.
type TraversalVL s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven traversal.
type TraversalVL' s a = TraversalVL s s a a

-- | Build a traversal from the van Laarhoven representation.
--
-- @
-- 'traversalVL' '.' 'traverseOf' ≡ 'id'
-- 'traverseOf' '.' 'traversalVL' ≡ 'id'
-- @
traversalVL :: TraversalVL s t a b -> Traversal s t a b
traversalVL t = Optic (wander t)
{-# INLINE traversalVL #-}

-- | Map each element of a structure targeted by a 'Traversal', evaluate these
-- actions from left to right, and collect the results.
traverseOf
  :: (Is k A_Traversal, Applicative f)
  => Optic k is s t a b
  -> (a -> f b) -> s -> f t
traverseOf o = \f -> runStar $ getOptic (castOptic @A_Traversal o) (Star f)
{-# INLINE traverseOf #-}

-- | A version of 'traverseOf' with the arguments flipped.
forOf
  :: (Is k A_Traversal, Applicative f)
  => Optic k is s t a b
  -> s -> (a -> f b) -> f t
forOf = flip . traverseOf
{-# INLINE forOf #-}

-- | Evaluate each action in the structure from left to right, and collect the
-- results.
--
-- >>> sequenceOf each ([1,2],[3,4])
-- [(1,3),(1,4),(2,3),(2,4)]
-- @
-- 'sequence' ≡ 'sequenceOf' 'traversed' ≡ 'traverse' 'id'
-- 'sequenceOf' o ≡ 'traverseOf' o 'id'
-- @
sequenceOf
  :: (Is k A_Traversal, Applicative f)
  => Optic k is s t (f b) b
  -> s -> f t
sequenceOf o = traverseOf o id
{-# INLINE sequenceOf #-}

-- | This generalizes 'Data.List.transpose' to an arbitrary 'Traversal'.
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
  :: Is k A_Traversal
  => Optic k is s t [a] a
  -> s -> [t]
transposeOf o = getZipList #. traverseOf o ZipList
{-# INLINE transposeOf #-}

-- | This generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'Traversal'.
--
-- @
-- 'Data.Traversable.mapAccumL' ≡ 'mapAccumLOf' 'traverse'
-- @
--
-- 'mapAccumLOf' accumulates 'State' from left to right.
mapAccumLOf
  :: Is k A_Traversal
  => Optic k is s t a b
  -> (acc -> a -> (b, acc)) -> acc -> s -> (t, acc)
mapAccumLOf o = \f acc0 s ->
  let g a = state $ \acc -> f acc a
  in runState (traverseOf o g s) acc0

{-# INLINE mapAccumLOf #-}

-- | This generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'Traversal'.
--
-- @
-- 'Data.Traversable.mapAccumR' ≡ 'mapAccumROf' 'traversed'
-- @
--
-- 'mapAccumROf' accumulates 'State' from right to left.
mapAccumROf
  :: Is k A_Traversal
  => Optic k is s t a b
  -> (acc -> a -> (b, acc)) -> acc -> s -> (t, acc)
mapAccumROf = mapAccumLOf . backwards
{-# INLINE mapAccumROf #-}

-- | This permits the use of 'scanl1' over an arbitrary 'Traversal'.
--
-- @
-- 'scanl1' ≡ 'scanl1Of' 'traversed'
-- @
scanl1Of
  :: Is k A_Traversal
  => Optic k is s t a a
  -> (a -> a -> a) -> s -> t
scanl1Of o = \f ->
  let step Nothing a  = (a, Just a)
      step (Just s) a = let r = f s a in (r, Just r)
  in fst . mapAccumLOf o step Nothing
{-# INLINE scanl1Of #-}

-- | This permits the use of 'scanr1' over an arbitrary 'Traversal'.
--
-- @
-- 'scanr1' ≡ 'scanr1Of' 'traversed'
-- @
scanr1Of
  :: Is k A_Traversal
  => Optic k is s t a a
  -> (a -> a -> a) -> s -> t
scanr1Of o = \f ->
  let step Nothing a  = (a, Just a)
      step (Just s) a = let r = f a s in (r, Just r)
  in fst . mapAccumROf o step Nothing
{-# INLINE scanr1Of #-}

-- | Try to map a function over this 'Traversal', returning Nothing if the
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
  :: Is k A_Traversal
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
  :: Is k A_Traversal
  => Optic k is s t a b
  -> (a -> b) -> s -> Maybe t
failover' o = \f s ->
  let OrT visited t = traverseOf o (wrapOrT . wrapIdentity' . f) s
  in if visited
     then Just (unwrapIdentity' t)
     else Nothing
{-# INLINE failover' #-}

----------------------------------------
-- Traversals

-- | Construct a 'Traversal' via the 'Traversable' class.
--
-- @
-- 'traverseOf' 'traversed' = 'traverse'
-- @
--
traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = Optic traversed__
{-# INLINE traversed #-}

----------------------------------------
-- Traversal combinators

-- | This allows you to 'traverse' the elements of a traversal in the opposite
-- order.
backwards
  :: Is k A_Traversal
  => Optic k is s t a b
  -> Traversal s t a b
backwards o = traversalVL $ \f -> forwards #. traverseOf o (Backwards #. f)
{-# INLINE backwards #-}

-- | 'partsOf' turns a 'Traversal' into a 'Lens'.
--
-- /Note:/ You should really try to maintain the invariant of the number of
-- children in the list.
--
-- >>> (a,b,c) & partsOf each .~ [x,y,z]
-- (x,y,z)
--
-- Any extras will be lost. If you do not supply enough, then the remainder will
-- come from the original structure.
--
-- >>> (a,b,c) & partsOf each .~ [w,x,y,z]
-- (w,x,y)
--
-- >>> (a,b,c) & partsOf each .~ [x,y]
-- (x,y,c)
--
-- >>> ('b', 'a', 'd', 'c') & partsOf each %~ sort
-- ('a','b','c','d')
--
-- So technically, this is only a 'Lens' if you do not change the number of
-- results it returns.
partsOf
  :: forall k is s t a. Is k A_Traversal
  => Optic k is s t a a
  -> Lens s t [a] [a]
partsOf o = lensVL $ \f s -> evalState (traverseOf o update s)
  <$> f (toListOf (getting $ castOptic @A_Traversal o) s)
  where
    update a = get >>= \case
      a' : as' -> put as' >> pure a'
      []       ->            pure a
{-# INLINE partsOf #-}
