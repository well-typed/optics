{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module: Optics.Traversal
-- Description: Lifts an effectful operation on elements to act on structures.
--
-- A 'Traversal' lifts an effectful operation on elements to act on structures
-- containing those elements.
--
-- That is, given a function @op :: A -> F B@ where @F@ is 'Applicative', a
-- @'Traversal' S T A B@ can produce a function @S -> F T@ that applies @op@ to
-- all the @A@s contained in the @S@.
--
-- This can be seen as a generalisation of 'traverse', where the type @S@ does
-- not need to be a type constructor with @A@ as the last parameter.
--
-- A 'Lens' is a 'Traversal' that acts on a single value.
--
-- A close relative is the 'Optics.AffineTraversal.AffineTraversal', which is a
-- 'Traversal' that acts on at most one value.
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
  , both

  -- * Additional elimination forms
  , forOf
  , sequenceOf
  , transposeOf
  , mapAccumROf
  , mapAccumLOf
  , scanr1Of
  , scanl1Of
  , rewriteMOf
  , transformMOf
  , failover
  , failover'

    -- * Combinators
  , backwards
  , partsOf
  , singular

  -- * Monoid structure
  -- | 'Traversal' admits a (partial) monoid structure where 'adjoin' combines
  -- non-overlapping traversals, and the identity element is
  -- 'Optics.IxAffineTraversal.ignored' (which traverses no elements).
  --
  -- If you merely need a 'Fold', you can use traversals as folds and combine
  -- them with one of the monoid structures on folds (see
  -- "Optics.Fold#monoids"). In particular, 'summing' can be used to concatenate
  -- results from two traversals, and 'failing' will returns results from the
  -- second traversal only if the first returns no results.
  --
  -- There is no 'Semigroup' or 'Monoid' instance for 'Traversal', because there
  -- is not a unique choice of monoid to use that works for all optics, and the
  -- ('<>') operator could not be used to combine optics of different kinds.
  , adjoin
  , failing

  -- * Subtyping
  , A_Traversal
  -- | <<diagrams/Traversal.png Traversal in the optics hierarchy>>

  -- * van Laarhoven encoding
  -- | The van Laarhoven representation of a 'Traversal' directly expresses how
  -- it lifts an effectful operation @A -> F B@ on elements to act on structures
  -- @S -> F T@.  Thus 'traverseOf' converts a 'Traversal' to a 'TraversalVL'.
  , TraversalVL
  , TraversalVL'
  )
  where

import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad.Trans.State
import Data.Bitraversable
import Data.Functor.Identity

import Data.Profunctor.Indexed
import Optics.AffineTraversal
import Optics.Fold
import Optics.Internal.Optic
import Optics.Internal.Traversal
import Optics.Internal.Utils
import Optics.Lens
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
--
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

-- | Rewrite by applying a monadic rule everywhere you recursing with a
-- user-specified 'Traversal'.
--
-- Ensures that the rule cannot be applied anywhere in the result.
--
-- @since 0.4.1
rewriteMOf
  :: (Is k A_Traversal, Monad m)
  => Optic k is a b a b
  -> (b -> m (Maybe a)) -> a -> m b
rewriteMOf l f = go
  where
    go = transformMOf l (\x -> f x >>= maybe (return x) go)
{-# INLINE rewriteMOf #-}

-- | Transform every element in a tree using a user supplied 'Traversal' in a
-- bottom-up manner with a monadic effect.
--
-- @since 0.4.1
transformMOf
  :: (Is k A_Traversal, Monad m)
  => Optic k is a b a b
  -> (b -> m b) -> a -> m b
transformMOf l f = go
  where
    go t = traverseOf l go t >>= f
{-# INLINE transformMOf #-}

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

-- | Traverse both parts of a 'Bitraversable' container with matching types.
--
-- /Note:/ for traversing a pair or an 'Either' it's better to use
-- 'Optics.Each.Core.each' and 'Optics.IxLens.chosen' respectively to reduce
-- potential for bugs due to too much polymorphism.
--
-- >>> (1,2) & both %~ (*10)
-- (10,20)
--
-- >>> over both length ("hello","world")
-- (5,5)
--
-- >>> foldOf both ("hello","world")
-- "helloworld"
--
-- @since 0.4
--
both :: Bitraversable r => Traversal (r a a) (r b b) a b
both = traversalVL $ \f -> bitraverse f f
{-# INLINE both #-}

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

-- | Convert a traversal to an 'AffineTraversal' that visits the first element
-- of the original traversal.
--
-- For the fold version see 'Optics.Fold.pre'.
--
-- >>> "foo" & singular traversed .~ 'z'
-- "zoo"
--
-- @since 0.3
singular
  :: forall k is s a. Is k A_Traversal
  => Optic' k is s a
  -> AffineTraversal' s a
singular o = atraversalVL $ \point f s ->
  case headOf (castOptic @A_Traversal o) s of
    Nothing -> point s
    Just a  -> evalState (traverseOf o update s) . Just <$> f a
  where
    update a = get >>= \case
      Just a' -> put Nothing >> pure a'
      Nothing ->                pure a
{-# INLINE singular #-}

failing
  :: (Is k A_Traversal, Is l A_Traversal)
  => Optic k is s t a b
  -> Optic l js s t a b
  -> Traversal s t a b
failing a b = traversalVL $ \f s ->
  let OrT visited fu = traverseOf a (wrapOrT . f) s
  in if visited
     then fu
     else traverseOf b f s
infixl 3 `failing` -- Same as (<|>)
{-# INLINE failing #-}


-- | Combine two disjoint traversals into one.
--
-- >>> over (_1 % _Just `adjoin` _2 % _Right) not (Just True, Right False)
-- (Just False,Right True)
--
-- /Note:/ if the argument traversals are not disjoint, the result will not
-- respect the 'Traversal' laws, because it will visit the same element multiple
-- times.  See section 7 of
-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/uitbaf.pdf Understanding Idiomatic Traversals Backwards and Forwards>
-- by Bird et al. for why this is illegal.
--
-- >>> view (partsOf (each `adjoin` _1)) ('x','y')
-- "xyx"
-- >>> set (partsOf (each `adjoin` _1)) "abc" ('x','y')
-- ('c','b')
--
-- For the 'Fold' version see 'Optics.Fold.summing'.
--
-- @since 0.4
--
adjoin
  :: (Is k A_Traversal, Is l A_Traversal)
  => Optic' k is s a
  -> Optic' l js s a
  -> Traversal' s a
adjoin o1 o2 = combined % traversed
  where
    combined = traversalVL $ \f s0 ->
      (\r1 r2 ->
         let s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
         in s2
      )
      <$> f (toListOf (castOptic @A_Traversal o1) s0)
      <*> f (toListOf (castOptic @A_Traversal o2) s0)

    update a = get >>= \case
      a' : as' -> put as' >> pure a'
      []       ->            pure a
infixr 6 `adjoin` -- Same as (<>)
{-# INLINE [1] adjoin #-}

{-# RULES

"adjoin_12_3" forall o1 o2 o3. adjoin o1 (adjoin o2 o3) = adjoin3 o1 o2 o3
"adjoin_21_3" forall o1 o2 o3. adjoin (adjoin o1 o2) o3 = adjoin3 o1 o2 o3

"adjoin_13_4" forall o1 o2 o3 o4. adjoin o1 (adjoin3 o2 o3 o4) = adjoin4 o1 o2 o3 o4
"adjoin_31_4" forall o1 o2 o3 o4. adjoin (adjoin3 o1 o2 o3) o4 = adjoin4 o1 o2 o3 o4

#-}

-- | Triple 'adjoin' for optimizing multiple 'adjoin's with rewrite rules.
adjoin3
  :: (Is k1 A_Traversal, Is k2 A_Traversal, Is k3 A_Traversal)
  => Optic' k1 is1 s a
  -> Optic' k2 is2 s a
  -> Optic' k3 is3 s a
  -> Traversal' s a
adjoin3 o1 o2 o3 = combined % traversed
  where
    combined = traversalVL $ \f s0 ->
      (\r1 r2 r3 ->
         let s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
             s3 = evalState (traverseOf o3 update s2) r3
         in s3
      )
      <$> f (toListOf (castOptic @A_Traversal o1) s0)
      <*> f (toListOf (castOptic @A_Traversal o2) s0)
      <*> f (toListOf (castOptic @A_Traversal o3) s0)

    update a = get >>= \case
      a' : as' -> put as' >> pure a'
      []       ->            pure a
{-# INLINE [1] adjoin3 #-}

{-# RULES

"adjoin_211_4" forall o1 o2 o3 o4. adjoin3 (adjoin o1 o2) o3 o4 = adjoin4 o1 o2 o3 o4
"adjoin_121_4" forall o1 o2 o3 o4. adjoin3 o1 (adjoin o2 o3) o4 = adjoin4 o1 o2 o3 o4
"adjoin_112_4" forall o1 o2 o3 o4. adjoin3 o1 o2 (adjoin o3 o4) = adjoin4 o1 o2 o3 o4

#-}

-- | Quadruple 'adjoin' for optimizing multiple 'adjoin's with rewrite rules.
adjoin4
  :: (Is k1 A_Traversal, Is k2 A_Traversal, Is k3 A_Traversal, Is k4 A_Traversal)
  => Optic' k1 is1 s a
  -> Optic' k2 is2 s a
  -> Optic' k3 is3 s a
  -> Optic' k4 is4 s a
  -> Traversal' s a
adjoin4 o1 o2 o3 o4 = combined % traversed
  where
    combined = traversalVL $ \f s0 ->
      (\r1 r2 r3 r4 ->
         let s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
             s3 = evalState (traverseOf o3 update s2) r3
             s4 = evalState (traverseOf o4 update s3) r4
         in s4
      )
      <$> f (toListOf (castOptic @A_Traversal o1) s0)
      <*> f (toListOf (castOptic @A_Traversal o2) s0)
      <*> f (toListOf (castOptic @A_Traversal o3) s0)
      <*> f (toListOf (castOptic @A_Traversal o4) s0)

    update a = get >>= \case
      a' : as' -> put as' >> pure a'
      []       ->            pure a
{-# INLINE [1] adjoin4 #-}

-- $setup
-- >>> import Data.List
-- >>> import Optics.Core
