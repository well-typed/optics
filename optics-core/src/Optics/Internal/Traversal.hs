{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Optics.Internal.Traversal where

import Optics.Internal.Optic

-- | Tag for a traversal.
data A_Traversal

-- | Constraints corresponding to a traversal.
type instance Constraints A_Traversal p f = (p ~ (->), Applicative f)

-- | Type synonym for a type-modifying traversal.
type Traversal s t a b = Optic A_Traversal s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal' s a = Optic' A_Traversal s a

-- | Explicitly cast an optic to a traversal.
toTraversal :: Is k A_Traversal => Optic k s t a b -> Traversal s t a b
toTraversal = sub
{-# INLINE toTraversal #-}

-- | Build a traversal from the van Laarhoven representation.
vlTraversal :: (forall f . Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
vlTraversal = Optic
{-# INLINE vlTraversal #-}

-- | Traversal via the 'Traversal' class.
--
-- TODO: This function is not necessary in 'lens', one can simply
-- use 'traverse'. The name here is preliminary.
--
_traverse :: Traversable t => Traversal (t a) (t b) a b
_traverse = vlTraversal traverse
{-# INLINE _traverse #-}

-- | Convert a traversal to the van Laarhoven representation.
traversalOf :: forall k s t a b . Is k A_Traversal => Optic k s t a b
            -> (forall f . Applicative f => (a -> f b) -> s -> f t)
traversalOf = getOptic . toTraversal
