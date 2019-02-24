{-# LANGUAGE DataKinds #-}
module Optics.IxTraversal
  ( A_Traversal
  , IxTraversal
  , IxTraversal'
  , TraversableWithIndex(..)
  , toIxTraversal
  , ixTraversalVL
  , itraverseOf
  , iforOf
  , imapAccumLOf
  , imapAccumROf
  , iscanl1Of
  , iscanr1Of
  , ifailover
  , ifailover'
  -- * Traversals
  , itraversed
  , ignored
  -- * Combinators
  , ibackwards
  , elementsOf
  , elements
  , elementOf
  , element
  , ipartsOf
  , module Optics.Optic
  ) where

import Control.Applicative.Backwards
import Control.Monad.Trans.State
import Data.Functor.Identity

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.IxTraversal
import Optics.Internal.Profunctor
import Optics.Internal.Optic
import Optics.Internal.Utils
import Optics.Optic
import Optics.Lens
import Optics.IxFold
import Optics.Traversal

-- | Type synonym for a type-modifying indexed traversal.
type IxTraversal i s t a b = Optic A_Traversal (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed traversal.
type IxTraversal' i s a = Optic' A_Traversal (WithIx i) s a

-- | Explicitly cast an optic to an indexed traversal.
toIxTraversal
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxTraversal i s t a b
toIxTraversal = castOptic
{-# INLINE toIxTraversal #-}

-- | Build an indexed traversal from the van Laarhoven representation.
--
-- @
-- 'ixTraversalVL' '.' 'itraverseOf' ≡ 'id'
-- 'itraverseOf' '.' 'ixTraversalVL' ≡ 'id'
-- @
ixTraversalVL
  :: (forall f. Applicative f => (i -> a -> f b) -> s -> f t)
  -> IxTraversal i s t a b
ixTraversalVL t = Optic (iwander t)
{-# INLINE ixTraversalVL #-}

----------------------------------------

itraverseOf
  :: (Is k A_Traversal, Applicative f, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> f b) -> s -> f t
itraverseOf o = \f -> runIxStar (getOptic (toIxTraversal o) (IxStar f)) id
{-# INLINE itraverseOf #-}

-- | A version of 'itraverseOf' with the arguments flipped.
iforOf
  :: (Is k A_Traversal, Applicative f, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> s -> (i -> a -> f b) -> f t
iforOf = flip . itraverseOf
{-# INLINE iforOf #-}

-- | Generalizes 'Data.Traversable.mapAccumL' to an arbitrary 'IXTraversal'.
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
-- retuning Nothing if the 'IxTraversal' has no targets.
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

-- | Version of 'ifailover' strict in the application of @f@.
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
-- >>> iover (itraversed <%> itraversed) (,) ["ab", "cd"]
-- [[((0,0),'a'),((0,1),'b')],[((1,0),'c'),((1,1),'d')]]
--
itraversed
  :: TraversableWithIndex i f
  => IxTraversal i (f a) (f b) a b
itraversed = Optic itraversed__
{-# INLINE itraversed #-}

-- | This is the trivial empty 'IxTraversal'.
--
-- >>> 6 & ignored %~ absurd
-- 6
ignored :: IxTraversal i s s a b
ignored = ixTraversalVL $ \_ -> pure

----------------------------------------
-- Traversal combinators

-- | This allows you to 'traverse' the elements of an indexed traversal in the
-- opposite order.
ibackwards
  :: (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> IxTraversal i s t a b
ibackwards o = Optic $ conjoined__ (backwards o) $ ixTraversalVL $ \f ->
  forwards #. itraverseOf o (\i -> Backwards #. f i)
{-# INLINE ibackwards #-}

-- Traverse selected elements of a 'Traversal' where their ordinal positions
-- match a predicate.
elementsOf
  :: Is k A_Traversal
  => Optic k is s t a a
  -> (Int -> Bool)
  -> IxTraversal Int s t a a
elementsOf o = \p -> ixTraversalVL $ \f ->
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
--
-- TODO: the result ideally should be an indexed affine traversal.
elementOf
  :: Is k A_Traversal
  => Optic k is s t a a
  -> Int
  -> IxTraversal Int s t a a
elementOf o = \i -> elementsOf o (== i)
{-# INLINE elementOf #-}

-- | Traverse the /nth/ element of a 'Traversable' container.
--
-- @
-- 'element' ≡ 'elementOf' 'traversed'
-- @
element :: Traversable f => Int -> IxTraversal' Int (f a) a
element = elementOf traversed
{-# INLINE element #-}

-- | An indexed version of 'partsOf' that receives the list of values paired
-- with their indices.
ipartsOf
  :: forall k is i s t a. (Is k A_Traversal, is `HasSingleIndex` i)
  => Optic k is s t a a
  -> Lens s t [(i, a)] [a]
ipartsOf o = lensVL $ \f s ->
  evalState (traverseOf o update s) <$> f (itoListOf fo s)
  where
    fo :: IxFold i s a
    fo = Optic $ rphantom . getOptic (toIxTraversal o)

    update a = get >>= \case
      []       ->            pure a
      a' : as' -> put as' >> pure a'
{-# INLINE ipartsOf #-}
