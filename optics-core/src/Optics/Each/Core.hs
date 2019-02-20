{-# LANGUAGE UndecidableInstances #-}
module Optics.Each.Core
  (
  -- * Each
    Each(..)
  ) where

import Data.Array
import Data.Complex
import Data.Functor.Identity
import Data.IntMap as IntMap
import Data.List.NonEmpty
import Data.Map as Map
import Data.Sequence as Seq
import Data.Tree as Tree

import Optics.IxTraversal

-- | Extract 'each' element of a (potentially monomorphic) container.
--
-- >>> over each (*10) (1,2,3)
-- (10,20,30)
--
-- >>> iover each (\i a -> a*10 + succ i) (1,2,3)
-- (11,22,33)
--
class Each i s t a b | s -> i a, t -> i b, s b -> t, t a -> s where
  each :: IxTraversal i s t a b

  default each
    :: (TraversableWithIndex i g, s ~ g a, t ~ g b)
    => IxTraversal i s t a b
  each = itraversed
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a) (b, b) a b@
instance
  (a ~ a1,
   b ~ b1
  ) => Each Int (a, a1)
                (b, b1) a b where
  each = ixTraversalVL $ \f ~(a0, a1) ->
    (,) <$> f 0 a0 <*> f 1 a1
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a, a) (b, b, b) a b@
instance
  (a ~ a1, a ~ a2,
   b ~ b1, b ~ b2
  ) => Each Int (a, a1, a2)
                (b, b1, b2) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2) ->
    (,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a, a, a) (b, b, b, b) a b@
instance
  (a ~ a1, a ~ a2, a ~ a3,
   b ~ b1, b ~ b2, b ~ b3
  ) => Each Int (a, a1, a2, a3)
                (b, b1, b2, b3) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3) ->
    (,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a, a, a, a) (b, b, b, b, b) a b@
instance
  (a ~ a1, a ~ a2, a ~ a3, a ~ a4,
   b ~ b1, b ~ b2, b ~ b3, b ~ b4
  ) => Each Int (a, a1, a2, a3, a4)
                (b, b1, b2, b3, b4) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3, a4) ->
    (,,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3 <*> f 4 a4
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a, a, a, a, a) (b, b, b, b, b, b) a b@
instance
  (a ~ a1, a ~ a2, a ~ a3, a ~ a4, a ~ a5,
   b ~ b1, b ~ b2, b ~ b3, b ~ b4, b ~ b5
  ) => Each Int (a, a1, a2, a3, a4, a5)
                (b, b1, b2, b3, b4, b5) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3, a4, a5) ->
    (,,,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3 <*> f 4 a4
            <*> f 5 a5
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a, a, a, a, a, a) (b, b, b, b, b, b, b) a
-- b@
instance
  (a ~ a1, a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6,
   b ~ b1, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6
  ) => Each Int (a, a1, a2, a3, a4, a5, a6)
                (b, b1, b2, b3, b4, b5, b6) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3, a4, a5, a6) ->
    (,,,,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3 <*> f 4 a4
             <*> f 5 a5 <*> f 6 a6
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' 'Int' (a, a, a, a, a, a, a, a) (b, b, b, b, b, b, b,
-- b) a b@
instance
  (a ~ a1, a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7,
   b ~ b1, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7
  ) => Each Int (a, a1, a2, a3, a4, a5, a6, a7)
                (b, b1, b2, b3, b4, b5, b6, b7) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3, a4, a5, a6, a7) ->
    (,,,,,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3 <*> f 4 a4
              <*> f 5 a5 <*> f 6 a6 <*> f 7 a7
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'Int' (a, a, a, a, a, a, a, a, a) (b, b, b, b, b, b,
-- b, b, b) a b@
instance
  (a ~ a1, a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8,
   b ~ b1, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8
  ) => Each Int (a, a1, a2, a3, a4, a5, a6, a7, a8)
                (b, b1, b2, b3, b4, b5, b6, b7, b8) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3, a4, a5, a6, a7, a8) ->
    (,,,,,,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3 <*> f 4 a4
               <*> f 5 a5 <*> f 6 a6 <*> f 7 a7 <*> f 8 a8
  {-# INLINE each #-}

-- | @'each' :: 'Traversal' 'Int' (a, a, a, a, a, a, a, a, a, a) (b, b, b, b, b,
-- b, b, b, b, b) a b@
instance
  (a ~ a1, a ~ a2, a ~ a3, a ~ a4, a ~ a5, a ~ a6, a ~ a7, a ~ a8, a ~ a9,
   b ~ b1, b ~ b2, b ~ b3, b ~ b4, b ~ b5, b ~ b6, b ~ b7, b ~ b8, b ~ b9
  ) => Each Int (a, a1, a2, a3, a4, a5, a6, a7, a8, a9)
                (b, b1, b2, b3, b4, b5, b6, b7, b8, b9) a b where
  each = ixTraversalVL $ \f ~(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) ->
    (,,,,,,,,,) <$> f 0 a0 <*> f 1 a1 <*> f 2 a2 <*> f 3 a3 <*> f 4 a4
                <*> f 5 a5 <*> f 6 a6 <*> f 7 a7 <*> f 8 a8 <*> f 9 a9
  {-# INLINE each #-}

-- | @'each' :: ('RealFloat' a, 'RealFloat' b) => 'IxTraversal' (Either () ())
-- ('Complex' a) ('Complex' b) a b@
instance Each (Either () ()) (Complex a) (Complex b) a b where
  each = ixTraversalVL $ \f (a :+ b) -> (:+) <$> f (Left ()) a <*> f (Right ()) b
  {-# INLINE each #-}

-- | @'each' :: 'IxTraversal' k ('Map' k a) ('Map' k b) a b@
instance k ~ k' => Each k (Map k a) (Map k' b) a b

-- | @'each' :: 'IxTraversal' 'Int' ('IntMap' a) ('IntMap' b) a b@
instance Each Int (IntMap a) (IntMap b) a b

-- | @'each' :: 'IxTraversal' 'Int' [a] [b] a b@
instance Each Int [a] [b] a b

-- | @'each' :: 'IxTraversal' 'Int' (NonEmpty a) (NonEmpty b) a b@
instance Each Int (NonEmpty a) (NonEmpty b) a b

-- | @'each' :: 'IxTraversal' () ('Identity' a) ('Identity' b) a b@
instance Each () (Identity a) (Identity b) a b

-- | @'each' :: 'IxTraversal' () ('Maybe' a) ('Maybe' b) a b@
instance Each () (Maybe a) (Maybe b) a b

-- | @'each' :: 'IxTraversal' 'Int' ('Seq' a) ('Seq' b) a b@
instance Each Int (Seq a) (Seq b) a b

-- | @'each' :: 'IxTraversal' [Int] ('Tree' a) ('Tree' b) a b@
instance Each [Int] (Tree a) (Tree b) a b

-- | @'each' :: 'Ix' i => 'IxTraversal' i ('Array' i a) ('Array' i b) a b@
instance (Ix i, i ~ j) => Each i (Array i a) (Array j b) a b
