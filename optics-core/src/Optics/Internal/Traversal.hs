{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Optics.Internal.Traversal where

import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for a type-modifying traversal.
type Traversal s t a b = Optic A_Traversal s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal' s a = Optic' A_Traversal s a

-- | Explicitly cast an optic to a traversal.
toTraversal :: Is k A_Traversal => Optic k s t a b -> Traversal s t a b
toTraversal = sub
{-# INLINE toTraversal #-}

-- | Build a traversal from the van Laarhoven representation.
vlTraversal :: (forall f. Applicative f => (a -> f b) -> s -> f t)
            -> Traversal s t a b
vlTraversal t = Optic (wander t)
{-# INLINE vlTraversal #-}

-- | Traversal via the 'Traversable' class.
traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = vlTraversal traverse
{-# INLINE traversed #-}

-- | Map each element of a structure targeted by a 'Traversal', evaluate these
-- actions from left to right, and collect the results.
traverseOf :: (Is k A_Traversal) => Optic k s t a b
           -> (forall f. Applicative f => (a -> f b) -> s -> f t)
traverseOf o = runStar #. getOptic (toTraversal o) .# Star
{-# INLINE traverseOf #-}
