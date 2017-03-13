{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
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

-- | Create a traversal.
mkTraversal :: Optic_ A_Traversal s t a b -> Traversal s t a b
mkTraversal = Optic
{-# INLINE mkTraversal #-}

-- traversalOf
