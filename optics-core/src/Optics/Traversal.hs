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
  , elementsOf
  -- * van Laarhoven encoding
  , TraversalVL
  , TraversalVL'
  , traversalVL
  -- * Re-exports
  , module Optics.Optic
  )
  where

import Optics.Internal.Optic
import Optics.Internal.Traversal
import Optics.Optic
