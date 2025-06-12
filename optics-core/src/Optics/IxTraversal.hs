{-# LANGUAGE DataKinds #-}
-- |
-- Module: Optics.IxTraversal
-- Description: An indexed version of a 'Optics.Traversal.Traversal'.
--
-- An 'IxTraversal' is an indexed version of a 'Optics.Traversal.Traversal'.
-- See the "Indexed optics" section of the overview documentation in the
-- @Optics@ module of the main @optics@ package for more details on indexed
-- optics.
--
module Optics.IxTraversal
  (
  -- * Formation
    IxTraversal
  , IxTraversal'

  -- * Introduction
  , itraversalVL

  -- * Elimination
  , itraverseOf

  -- * Computation
  -- |
  --
  -- @
  -- 'itraverseOf' ('itraversalVL' f) ≡ f
  -- @

  -- * Well-formedness
  -- |
  --
  -- @
  -- 'itraverseOf' o ('const' 'pure') ≡ 'pure'
  -- 'fmap' ('itraverseOf' o f) . 'itraverseOf' o g ≡ 'Data.Functor.Compose.getCompose' . 'itraverseOf' o (\\ i -> 'Data.Functor.Compose.Compose' . 'fmap' (f i) . g i)
  -- @
  --

  -- * Additional introduction forms
  -- | See also 'Optics.Each.Core.each', which is an 'IxTraversal' over each element of a (potentially monomorphic) container.
  , itraversed
  , ignored
  , elementsOf
  , elements
  , elementOf
  , element

  -- * Additional elimination forms
  , iforOf
  , imapAccumLOf
  , imapAccumROf
  , iscanl1Of
  , iscanr1Of
  , ifailover
  , ifailover'

  -- * Combinators
  , indices
  , ibackwards
  , ipartsOf
  , isingular

  -- * Monoid structure
  -- | 'IxTraversal' admits a (partial) monoid structure where 'iadjoin'
  -- combines non-overlapping indexed traversals, and the identity element is
  -- 'ignored' (which traverses no elements).
  --
  -- If you merely need an 'IxFold', you can use indexed traversals as indexed
  -- folds and combine them with one of the monoid structures on indexed folds
  -- (see "Optics.IxFold#monoids"). In particular, 'isumming' can be used to
  -- concatenate results from two traversals, and 'ifailing' will returns
  -- results from the second traversal only if the first returns no results.
  --
  -- There is no 'Semigroup' or 'Monoid' instance for 'IxTraversal', because
  -- there is not a unique choice of monoid to use that works for all optics,
  -- and the ('<>') operator could not be used to combine optics of different
  -- kinds.
  , iadjoin
  , ifailing

  -- * Subtyping
  , A_Traversal

  -- * van Laarhoven encoding
  -- | The van Laarhoven representation of an 'IxTraversal' directly expresses
  -- how it lifts an effectful operation @I -> A -> F B@ on elements and their
  -- indices to act on structures @S -> F T@.  Thus 'itraverseOf' converts an
  -- 'IxTraversal' to an 'IxTraversalVL'.
  , IxTraversalVL
  , IxTraversalVL'

  -- * Re-exports
  , TraversableWithIndex(..)
  ) where

import Control.Applicative.Backwards
import Control.Monad.Trans.State
import Data.Functor.Identity

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Indexed.Classes
import Optics.Internal.IxTraversal
import Optics.Internal.Optic
import Optics.Internal.Utils
import Optics.IxAffineTraversal
import Optics.IxLens
import Optics.IxFold
import Optics.ReadOnly
import Optics.Traversal

-- | Type synonym for a type-modifying indexed traversal.
type IxTraversal i s t a b = Optic A_Traversal (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed traversal.
type IxTraversal' i s a = Optic' A_Traversal (WithIx i) s a

-- | Type synonym for a type-modifying van Laarhoven indexed traversal.
type IxTraversalVL i s t a b =
  forall f. Applicative f => (i -> a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven indexed traversal.
type IxTraversalVL' i s a = IxTraversalVL i s s a a

-- | Build an indexed traversal from the van Laarhoven representation.
--
-- @
-- 'itraversalVL' '.' 'itraverseOf' ≡ 'id'
-- 'itraverseOf' '.' 'itraversalVL' ≡ 'id'
-- @
itraversalVL :: IxTraversalVL i s t a b -> IxTraversal i s t a b
itraversalVL t = Optic (iwander t)
{-# INLINE itraversalVL #-}

----------------------------------------

-- | Map each element of a structure targeted by an 'IxTraversal' (supplying the
-- index), evaluate these actions from left to right, and collect the results.
--
-- This yields the van Laarhoven representation of an indexed traversal.
itraverseOf
  :: (Is k A_Traversal, Applicative f, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> f b) -> s -> f t
itraverseOf o = \f ->
  runIxStar (getOptic (castOptic @A_Traversal o) (IxStar f)) id
{-# INLINE itraverseOf #-}

-- | A version of 'itraverseOf' with the arguments flipped.
iforOf
  :: (Is k A_Traversal, Applicative f, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> s -> (i -> a -> f b) -> f t
iforOf = flip . itraverseOf
{-# INLINE iforOf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IxTraversal'.
--
-- 'imapAccumLOf' accumulates state from left to right.
--
-- @
-- 'Optics.Traversal.mapAccumLOf' o ≡ 'imapAccumLOf' o '.' 'const'
-- @
imapAccumLOf
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> acc -> a -> (b, acc)) -> acc -> s -> (t, acc)
imapAccumLOf o = \f acc0 s ->
  let g i a = state $ \acc -> f i acc a
  in runState (itraverseOf o g s) acc0
{-# INLINE imapAccumLOf #-}

-- | Generalizes 'Data.Traversable.mapAccumR' to an arbitrary 'IxTraversal'.
--
-- 'imapAccumROf' accumulates state from right to left.
--
-- @
-- 'Optics.Traversal.mapAccumROf' o ≡ 'imapAccumROf' o '.' 'const'
-- @
imapAccumROf
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> acc -> a -> (b, acc)) -> acc -> s -> (t, acc)
imapAccumROf = imapAccumLOf . ibackwards
{-# INLINE imapAccumROf #-}

-- | This permits the use of 'scanl1' over an arbitrary 'IxTraversal'.
iscanl1Of
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a a
  -> (i -> a -> a -> a) -> s -> t
iscanl1Of o = \f ->
  let step i ms a = case ms of
        Nothing -> (a, Just a)
        Just s  -> let r = f i s a in (r, Just r)
  in fst . imapAccumLOf o step Nothing
{-# INLINE iscanl1Of #-}

-- | This permits the use of 'scanr1' over an arbitrary 'IxTraversal'.
iscanr1Of
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a a
  -> (i -> a -> a -> a) -> s -> t
iscanr1Of o f = fst . imapAccumROf o step Nothing
  where
    step i ms a = case ms of
      Nothing -> (a, Just a)
      Just s  -> let r = f i a s in (r, Just r)
{-# INLINE iscanr1Of #-}

-- | Try to map a function which uses the index over this 'IxTraversal',
-- returning 'Nothing' if the 'IxTraversal' has no targets.
ifailover
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> Maybe t
ifailover o = \f s ->
  let OrT visited t = itraverseOf o (\i -> wrapOrT . Identity #. f i) s
  in if visited
     then Just (runIdentity t)
     else Nothing
{-# INLINE ifailover #-}

-- | Version of 'ifailover' strict in the application of the function.
ifailover'
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> Maybe t
ifailover' o = \f s ->
  let OrT visited t = itraverseOf o (\i -> wrapOrT . wrapIdentity' . f i) s
  in if visited
     then Just (unwrapIdentity' t)
     else Nothing
{-# INLINE ifailover' #-}

----------------------------------------
-- Traversals

-- | Indexed traversal via the 'TraversableWithIndex' class.
--
-- @
-- 'itraverseOf' 'itraversed' ≡ 'itraverse'
-- @
--
-- >>> iover (itraversed <%> itraversed) (,) ["ab", "cd"]
-- [[((0,0),'a'),((0,1),'b')],[((1,0),'c'),((1,1),'d')]]
--
itraversed
  :: TraversableWithIndex i f
  => IxTraversal i (f a) (f b) a b
itraversed = Optic itraversed__
{-# INLINE itraversed #-}

----------------------------------------
-- Traversal combinators

-- | Filter results of an 'IxTraversal' that don't satisfy a predicate on the
-- indices.
--
-- >>> toListOf (itraversed %& indices even) "foobar"
-- "foa"
--
indices
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => (i -> Bool)
  -> Optic k is s t a a
  -> IxTraversal i s t a a
indices p o = itraversalVL $ \f ->
  itraverseOf o $ \i a -> if p i then f i a else pure a
{-# INLINE indices #-}

-- | This allows you to 'traverse' the elements of an indexed traversal in the
-- opposite order.
ibackwards
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxTraversal i s t a b
ibackwards o = conjoined (backwards o) $ itraversalVL $ \f ->
  forwards #. itraverseOf o (\i -> Backwards #. f i)
{-# INLINE ibackwards #-}

-- | Traverse selected elements of a 'Traversal' where their ordinal positions
-- match a predicate.
elementsOf
  :: Is k A_Traversal
  => Optic k is s t a a
  -> (Int -> Bool)
  -> IxTraversal Int s t a a
elementsOf o = \p -> itraversalVL $ \f ->
  indexing (traverseOf o) $ \i a -> if p i then f i a else pure a
{-# INLINE elementsOf #-}

-- | Traverse elements of a 'Traversable' container where their ordinal
-- positions match a predicate.
--
-- @
-- 'elements' ≡ 'elementsOf' 'traverse'
-- @
elements :: Traversable f => (Int -> Bool) -> IxTraversal' Int (f a) a
elements = elementsOf traversed
{-# INLINE elements #-}

-- | Traverse the /nth/ element of a 'Traversal' if it exists.
elementOf
  :: Is k A_Traversal
  => Optic' k is s a
  -> Int
  -> IxAffineTraversal' Int s a
elementOf o = \i -> isingular $ elementsOf o (== i)
{-# INLINE elementOf #-}

-- | Traverse the /nth/ element of a 'Traversable' container.
--
-- @
-- 'element' ≡ 'elementOf' 'traversed'
-- @
element :: Traversable f => Int -> IxAffineTraversal' Int (f a) a
element = elementOf traversed
{-# INLINE element #-}

-- | An indexed version of 'partsOf' that receives the entire list of indices as
-- its indices.
ipartsOf
  :: forall k is i s t a. (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a a
  -> IxLens [i] s t [a] [a]
ipartsOf o = conjoined (partsOf o) $ ilensVL $ \f s ->
  evalState (traverseOf o update s)
    <$> uncurry' f (unzip $ itoListOf (getting $ castOptic @A_Traversal o) s)
  where
    update a = get >>= \case
      []       ->            pure a
      a' : as' -> put as' >> pure a'
{-# INLINE ipartsOf #-}

-- | Convert an indexed traversal to an 'IxAffineTraversal' that visits the
-- first element of the original traversal.
--
-- For the fold version see 'Optics.IxFold.ipre'.
--
-- >>> [1,2,3] & iover (isingular itraversed) (-)
-- [-1,2,3]
--
-- @since 0.3
isingular
  :: forall k is i s a. (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic' k is s a
  -> IxAffineTraversal' i s a
isingular o = conjoined (singular o) $ iatraversalVL $ \point f s ->
  case iheadOf (castOptic @A_Traversal o) s of
    Nothing     -> point s
    Just (i, a) -> evalState (traverseOf o update s) . Just <$> f i a
  where
    update a = get >>= \case
      Just a' -> put Nothing >> pure a'
      Nothing ->                pure a
{-# INLINE isingular #-}

ifailing
  :: ( Is k A_Traversal, Is l A_Traversal
     , is1 `HasSingleIndex` i, is2 `HasSingleIndex` i)
  => Optic k is1 s t a b
  -> Optic l is2 s t a b
  -> IxTraversal i s t a b
ifailing a b = conjoined (failing a b) $ itraversalVL $ \f s ->
  let OrT visited fu = itraverseOf a (\i -> wrapOrT . f i) s
  in if visited
     then fu
     else itraverseOf b f s
infixl 3 `ifailing` -- Same as (<|>)
{-# INLINE ifailing #-}

-- | Combine two disjoint indexed traversals into one.
--
-- >>> iover (_1 % itraversed `iadjoin` _2 % itraversed) (+) ([0, 0, 0], (3, 5))
-- ([0,1,2],(3,8))
--
-- /Note:/ if the argument traversals are not disjoint, the result will not
-- respect the 'IxTraversal' laws, because it will visit the same element multiple
-- times.  See section 7 of
-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/uitbaf.pdf Understanding Idiomatic Traversals Backwards and Forwards>
-- by Bird et al. for why this is illegal.
--
-- >>> iview (ipartsOf (each `iadjoin` each)) ("x","y")
-- ([0,1,0,1],["x","y","x","y"])
-- >>> iset (ipartsOf (each `iadjoin` each)) (const ["a","b","c","d"]) ("x","y")
-- ("c","d")
--
-- For the 'IxFold' version see 'Optics.IxFold.isumming'.
--
-- @since 0.4
--
iadjoin
  :: (Is k A_Traversal, Is l A_Traversal, is `HasSingleIndex` i)
  => Optic' k is s a
  -> Optic' l is s a
  -> IxTraversal' i s a
iadjoin o1 o2 = conjoined (adjoin o1 o2) (combined % traversed % itraversed)
  where
    combined = traversalVL $ \f s0 ->
      (\r1 r2 ->
         let s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
         in s2
      )
      <$> f (itoListOf (castOptic @A_Traversal o1) s0)
      <*> f (itoListOf (castOptic @A_Traversal o2) s0)

    update a = get >>= \case
      (_, a') : as' -> put as' >> pure a'
      []            ->            pure a
infixr 6 `iadjoin` -- Same as (<>)
{-# INLINE [1] iadjoin #-}

{-# RULES

"iadjoin_12_3" forall o1 o2 o3. iadjoin o1 (iadjoin o2 o3) = iadjoin3 o1 o2 o3
"iadjoin_21_3" forall o1 o2 o3. iadjoin (iadjoin o1 o2) o3 = iadjoin3 o1 o2 o3

"iadjoin_13_4" forall o1 o2 o3 o4. iadjoin o1 (iadjoin3 o2 o3 o4) = iadjoin4 o1 o2 o3 o4
"iadjoin_31_4" forall o1 o2 o3 o4. iadjoin (iadjoin3 o1 o2 o3) o4 = iadjoin4 o1 o2 o3 o4

#-}

-- | Triple 'iadjoin' for optimizing multiple 'iadjoin's with rewrite rules.
iadjoin3
  :: (Is k1 A_Traversal, Is k2 A_Traversal, Is k3 A_Traversal, is `HasSingleIndex` i )
  => Optic' k1 is s a
  -> Optic' k2 is s a
  -> Optic' k3 is s a
  -> IxTraversal' i s a
iadjoin3 o1 o2 o3 = conjoined (o1 `adjoin` o2 `adjoin` o3)
                              (combined % traversed % itraversed)
  where
    combined = traversalVL $ \f s0 ->
      (\r1 r2 r3 ->
         let s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
             s3 = evalState (traverseOf o3 update s2) r3
         in s3
      )
      <$> f (itoListOf (castOptic @A_Traversal o1) s0)
      <*> f (itoListOf (castOptic @A_Traversal o2) s0)
      <*> f (itoListOf (castOptic @A_Traversal o3) s0)

    update a = get >>= \case
      (_, a') : as' -> put as' >> pure a'
      []            ->            pure a
{-# INLINE [1] iadjoin3 #-}

{-# RULES

"iadjoin_211_4" forall o1 o2 o3 o4. iadjoin3 (iadjoin o1 o2) o3 o4 = iadjoin4 o1 o2 o3 o4
"iadjoin_121_4" forall o1 o2 o3 o4. iadjoin3 o1 (iadjoin o2 o3) o4 = iadjoin4 o1 o2 o3 o4
"iadjoin_112_4" forall o1 o2 o3 o4. iadjoin3 o1 o2 (iadjoin o3 o4) = iadjoin4 o1 o2 o3 o4

#-}

-- | Quadruple 'iadjoin' for optimizing multiple 'iadjoin's with rewrite rules.
iadjoin4
  :: ( Is k1 A_Traversal, Is k2 A_Traversal, Is k3 A_Traversal, Is k4 A_Traversal
     , is `HasSingleIndex` i)
  => Optic' k1 is s a
  -> Optic' k2 is s a
  -> Optic' k3 is s a
  -> Optic' k4 is s a
  -> IxTraversal' i s a
iadjoin4 o1 o2 o3 o4 = conjoined (o1 `adjoin` o2 `adjoin` o3 `adjoin` o4)
                                 (combined % traversed % itraversed)
  where
    combined = traversalVL $ \f s0 ->
      (\r1 r2 r3 r4 ->
         let s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
             s3 = evalState (traverseOf o3 update s2) r3
             s4 = evalState (traverseOf o4 update s3) r4
         in s4
      )
      <$> f (itoListOf (castOptic @A_Traversal o1) s0)
      <*> f (itoListOf (castOptic @A_Traversal o2) s0)
      <*> f (itoListOf (castOptic @A_Traversal o3) s0)
      <*> f (itoListOf (castOptic @A_Traversal o4) s0)

    update a = get >>= \case
      (_, a') : as' -> put as' >> pure a'
      []            ->            pure a
{-# INLINE [1] iadjoin4 #-}

-- $setup
-- >>> import Data.Void
-- >>> import Optics.Core
