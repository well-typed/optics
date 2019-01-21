module Optics.Traversal
  (
  -- * Formation
    A_Traversal
  , Traversal
  , Traversal'
  -- * Introduction
  , traversed
  , toTraversal
  -- * Elimination
  , traverseOf
  , forOf
  -- * van Laarhoven encoding
  , TraversalVL
  , TraversalVL'
  , traversalVL
  -- * Traversals
  , both
  , elementsOf
  , elementOf
  -- * Re-exports
  , module Optics.Optic
  )
  where

import Data.Bitraversable

import Optics.Internal.Optic
import Optics.Internal.Traversal
import Optics.Optic

-- | Traverse both parts of a 'Bitraversable' container with matching types.
--
-- Usually that type will be a pair.
--
-- >>> over both (1, 2) (*10)
-- (10,20)
--
-- >>> over both length ("hello","world")
-- (5,5)
--
-- >>> view both ("hello", "world")
-- "helloworld"
both :: Bitraversable r => Traversal (r a a) (r b b) a b
both = traversalVL $ \f -> bitraverse f f
{-# INLINE both #-}
