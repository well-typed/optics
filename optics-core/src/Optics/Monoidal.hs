{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.Monoidal where

import GHC.TypeLits
import Optics.Fold
import Optics.IxFold
import Optics.IxSetter
import Optics.IxTraversal
import Optics.Internal.Optic.Types
import Optics.Optic
import Optics.Setter
import Optics.Traversal

-- Essentially this collapses the hierarchy into
-- Traversal, Fold, and Setter,
-- and then takes the usual Join
type family AfterJoin (k :: OpticKind) (l :: OpticKind) where
  -- BEGIN GENERATED CONTENT
  -- An_Iso-----
  -- no AfterJoin with         A_ReversedLens
  AfterJoin An_Iso             A_ReversedPrism    = A_Fold
  AfterJoin An_Iso             A_Prism            = A_Traversal
  -- no AfterJoin with         A_Review
  AfterJoin An_Iso             A_Lens             = A_Traversal
  AfterJoin An_Iso             A_Getter           = A_Fold
  AfterJoin An_Iso             An_AffineTraversal = A_Traversal
  AfterJoin An_Iso             An_AffineFold      = A_Fold
  AfterJoin An_Iso             A_Traversal        = A_Traversal
  AfterJoin An_Iso             A_Fold             = A_Fold
  AfterJoin An_Iso             A_Setter           = A_Setter

  -- no AfterJoin with A_ReversedLens-----

  -- A_ReversedPrism-----
  AfterJoin A_ReversedPrism    An_Iso             = A_Fold
  -- no AfterJoin with         A_ReversedLens
  AfterJoin A_ReversedPrism    A_Prism            = A_Fold
  -- no AfterJoin with         A_Review
  AfterJoin A_ReversedPrism    A_Lens             = A_Fold
  AfterJoin A_ReversedPrism    A_Getter           = A_Fold
  AfterJoin A_ReversedPrism    An_AffineTraversal = A_Fold
  AfterJoin A_ReversedPrism    An_AffineFold      = A_Fold
  AfterJoin A_ReversedPrism    A_Traversal        = A_Fold
  AfterJoin A_ReversedPrism    A_Fold             = A_Fold
  -- no AfterJoin with         A_Setter

  -- A_Prism-----
  AfterJoin A_Prism            An_Iso             = A_Traversal
  -- no AfterJoin with         A_ReversedLens
  AfterJoin A_Prism            A_ReversedPrism    = A_Fold
  -- no AfterJoin with         A_Review
  AfterJoin A_Prism            A_Lens             = A_Traversal
  AfterJoin A_Prism            A_Getter           = A_Fold
  AfterJoin A_Prism            An_AffineTraversal = A_Traversal
  AfterJoin A_Prism            An_AffineFold      = A_Fold
  AfterJoin A_Prism            A_Traversal        = A_Traversal
  AfterJoin A_Prism            A_Fold             = A_Fold
  AfterJoin A_Prism            A_Setter           = A_Setter

  -- no AfterJoind with A_Review-----

  -- A_Lens-----
  AfterJoin A_Lens             An_Iso             = A_Traversal
  -- no AfterJoin with         A_ReversedLens
  AfterJoin A_Lens             A_ReversedPrism    = A_Fold
  AfterJoin A_Lens             A_Prism            = A_Traversal
  -- no AfterJoin with         A_Review
  AfterJoin A_Lens             A_Getter           = A_Fold
  AfterJoin A_Lens             An_AffineTraversal = A_Traversal
  AfterJoin A_Lens             An_AffineFold      = A_Fold
  AfterJoin A_Lens             A_Traversal        = A_Traversal
  AfterJoin A_Lens             A_Fold             = A_Fold
  AfterJoin A_Lens             A_Setter           = A_Setter

  -- A_Getter-----
  AfterJoin A_Getter           An_Iso             = A_Fold
  -- no AfterJoin with         A_ReversedLens
  AfterJoin A_Getter           A_ReversedPrism    = A_Fold
  AfterJoin A_Getter           A_Prism            = A_Fold
  -- no AfterJoin with         A_Review
  AfterJoin A_Getter           A_Lens             = A_Fold
  AfterJoin A_Getter           An_AffineTraversal = A_Fold
  AfterJoin A_Getter           An_AffineFold      = A_Fold
  AfterJoin A_Getter           A_Traversal        = A_Fold
  AfterJoin A_Getter           A_Fold             = A_Fold
  -- no AfterJoin with         A_Setter

  -- An_AffineTraversal-----
  AfterJoin An_AffineTraversal An_Iso             = A_Traversal
  -- no AfterJoin with         A_ReversedLens
  AfterJoin An_AffineTraversal A_ReversedPrism    = A_Fold
  AfterJoin An_AffineTraversal A_Prism            = A_Traversal
  -- no AfterJoin with         A_Review
  AfterJoin An_AffineTraversal A_Lens             = A_Traversal
  AfterJoin An_AffineTraversal A_Getter           = A_Fold
  AfterJoin An_AffineTraversal An_AffineFold      = A_Fold
  AfterJoin An_AffineTraversal A_Traversal        = A_Traversal
  AfterJoin An_AffineTraversal A_Fold             = A_Fold
  AfterJoin An_AffineTraversal A_Setter           = A_Setter

  -- An_AffineFold-----
  AfterJoin An_AffineFold      An_Iso             = A_Fold
  -- no AfterJoin with         A_ReversedLens
  AfterJoin An_AffineFold      A_ReversedPrism    = A_Fold
  AfterJoin An_AffineFold      A_Prism            = A_Fold
  -- no AfterJoin with         A_Review
  AfterJoin An_AffineFold      A_Lens             = A_Fold
  AfterJoin An_AffineFold      A_Getter           = A_Fold
  AfterJoin An_AffineFold      An_AffineTraversal = A_Fold
  AfterJoin An_AffineFold      A_Traversal        = A_Fold
  AfterJoin An_AffineFold      A_Fold             = A_Fold
  -- no AfterJoin with         A_Setter

  -- A_Traversal-----
  AfterJoin A_Traversal        An_Iso             = A_Traversal
  -- no AfterJoin with         A_ReversedLens
  AfterJoin A_Traversal        A_ReversedPrism    = A_Fold
  AfterJoin A_Traversal        A_Prism            = A_Traversal
  -- no AfterJoin with         A_Review
  AfterJoin A_Traversal        A_Lens             = A_Traversal
  AfterJoin A_Traversal        A_Getter           = A_Fold
  AfterJoin A_Traversal        An_AffineTraversal = A_Traversal
  AfterJoin A_Traversal        An_AffineFold      = A_Fold
  AfterJoin A_Traversal        A_Fold             = A_Fold
  AfterJoin A_Traversal        A_Setter           = A_Setter

  -- A_Fold-----
  AfterJoin A_Fold             An_Iso             = A_Fold
  -- no AfterJoin with         A_ReversedLens
  AfterJoin A_Fold             A_ReversedPrism    = A_Fold
  AfterJoin A_Fold             A_Prism            = A_Fold
  -- no AfterJoin with         A_Review
  AfterJoin A_Fold             A_Lens             = A_Fold
  AfterJoin A_Fold             A_Getter           = A_Fold
  AfterJoin A_Fold             An_AffineTraversal = A_Fold
  AfterJoin A_Fold             An_AffineFold      = A_Fold
  AfterJoin A_Fold             A_Traversal        = A_Fold
  -- no AfterJoin with         A_Setter

  -- no AfterJoin with A_Setter-----
  -- A_Traversal-----
  AfterJoin A_Setter           An_Iso             = A_Setter
  -- no AfterJoin with         A_ReversedLens
  -- no AfterJoin with         A_ReversedPrism
  AfterJoin A_Setter           A_Prism            = A_Setter
  -- no AfterJoin with         A_Review
  AfterJoin A_Setter           A_Lens             = A_Setter
  -- no AfterJoin with         A_Getter
  AfterJoin A_Setter           An_AffineTraversal = A_Setter
  -- no AfterJoin with         An_AffineFold
  -- no AfterJoin with         A_Fold

  -- END GENERATED CONTENT

  -- Every optic kinds can be joined with itself.
  AfterJoin k k = k

  -- Everything else is a type error.
  AfterJoin k l = TypeError ('ShowType k
                       ':<>: 'Text " and "
                       ':<>: 'ShowType l
                       ':<>: 'Text " cannot be composed sequentially")

infixr 6 <++>
class After is m where
  nothing :: Optic' m is s a
  (<++>)  :: (Is k m, Is l m, m ~ AfterJoin k l)
          => Optic' k is s a -> Optic' l is s a
          -> Optic' m is s a

instance After NoIx A_Traversal where
  nothing = traversalVL $ \_ s -> pure s
  t1 <++> t2 = traversalVL $ \f s -> traverseOf t1 f s *> traverseOf t2 f s
instance After (WithIx ix) A_Traversal where
  nothing = itraversalVL $ \_ s -> pure s
  t1 <++> t2 = itraversalVL $ \f s -> itraverseOf t1 f s *> itraverseOf t2 f s

instance After NoIx A_Fold where
  nothing = foldring (\_ r _ -> r)
  (<++>) = summing
instance After (WithIx ix) A_Fold where
  nothing = ifoldring (\_ r _ -> r)
  (<++>) = isumming

instance After NoIx A_Setter where
  nothing = sets (\_ s -> s)
  s1 <++> s2 = sets (\f s -> over s2 f (over s1 f s))
instance After (WithIx ix) A_Setter where
  nothing = isets (\_ s -> s)
  s1 <++> s2 = isets (\f s -> iover s2 f (iover s1 f s))