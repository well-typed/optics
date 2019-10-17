{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Instances to implement the subtyping hierarchy between optics.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Optic.Subtyping where

import GHC.TypeLits (ErrorMessage(..), TypeError)

import Optics.Internal.Optic.Types

-- | Subtyping relationship between kinds of optics.
--
-- An instance of @'Is' k l@ means that any @'Optics.Optic.Optic' k@ can be used
-- as an @'Optics.Optic.Optic' l@. For example, we have an @'Is' 'A_Lens'
-- 'A_Traversal'@ instance, but not @'Is' 'A_Traversal' 'A_Lens'@.
--
-- This class needs instances for all possible combinations of tags.
--
class Is k l where
  -- | Witness of the subtyping relationship.
  implies ::
    proxy k l p -> (Constraints k p => r) -> (Constraints l p => r)

-- | Overlappable instance for a custom type error.
instance {-# OVERLAPPABLE #-} TypeError ('ShowType k
                                         ':<>: 'Text " cannot be used as "
                                         ':<>: 'ShowType l
                                        ) => Is k l where
  implies = error "unreachable"

-- | Every kind of optic can be used as itself.
instance Is k k where
  implies _ = id

----------------------------------------

-- BEGIN GENERATED CONTENT

-- An_Iso
instance Is An_Iso             A_ReversedLens     where implies _ = id
instance Is An_Iso             A_ReversedPrism    where implies _ = id
instance Is An_Iso             A_Prism            where implies _ = id
instance Is An_Iso             A_Review           where implies _ = id
instance Is An_Iso             A_Lens             where implies _ = id
instance Is An_Iso             A_Getter           where implies _ = id
instance Is An_Iso             A_Traversal1       where implies _ = id
instance Is An_Iso             A_Fold1            where implies _ = id
instance Is An_Iso             An_AffineTraversal where implies _ = id
instance Is An_Iso             An_AffineFold      where implies _ = id
instance Is An_Iso             A_Traversal        where implies _ = id
instance Is An_Iso             A_Fold             where implies _ = id
instance Is An_Iso             A_Setter           where implies _ = id
-- A_ReversedLens
instance Is A_ReversedLens     A_Review           where implies _ = id
-- A_ReversedPrism
instance Is A_ReversedPrism    A_Getter           where implies _ = id
instance Is A_ReversedPrism    A_Fold1            where implies _ = id
instance Is A_ReversedPrism    An_AffineFold      where implies _ = id
instance Is A_ReversedPrism    A_Fold             where implies _ = id
-- A_Prism
instance Is A_Prism            A_Review           where implies _ = id
instance Is A_Prism            An_AffineTraversal where implies _ = id
instance Is A_Prism            An_AffineFold      where implies _ = id
instance Is A_Prism            A_Traversal        where implies _ = id
instance Is A_Prism            A_Fold             where implies _ = id
instance Is A_Prism            A_Setter           where implies _ = id
-- A_Lens
instance Is A_Lens             A_Getter           where implies _ = id
instance Is A_Lens             A_Traversal1       where implies _ = id
instance Is A_Lens             A_Fold1            where implies _ = id
instance Is A_Lens             An_AffineTraversal where implies _ = id
instance Is A_Lens             An_AffineFold      where implies _ = id
instance Is A_Lens             A_Traversal        where implies _ = id
instance Is A_Lens             A_Fold             where implies _ = id
instance Is A_Lens             A_Setter           where implies _ = id
-- A_Getter
instance Is A_Getter           A_Fold1            where implies _ = id
instance Is A_Getter           An_AffineFold      where implies _ = id
instance Is A_Getter           A_Fold             where implies _ = id
-- A_Traversal1
instance Is A_Traversal1       A_Fold1            where implies _ = id
instance Is A_Traversal1       A_Traversal        where implies _ = id
instance Is A_Traversal1       A_Fold             where implies _ = id
instance Is A_Traversal1       A_Setter           where implies _ = id
-- A_Fold1
instance Is A_Fold1            A_Fold             where implies _ = id
-- An_AffineTraversal
instance Is An_AffineTraversal An_AffineFold      where implies _ = id
instance Is An_AffineTraversal A_Traversal        where implies _ = id
instance Is An_AffineTraversal A_Fold             where implies _ = id
instance Is An_AffineTraversal A_Setter           where implies _ = id
-- An_AffineFold
instance Is An_AffineFold      A_Fold             where implies _ = id
-- A_Traversal
instance Is A_Traversal        A_Fold             where implies _ = id
instance Is A_Traversal        A_Setter           where implies _ = id

-- END GENERATED CONTENT

----------------------------------------

-- | Computes the least upper bound of two optics kinds.
--
-- @Join k l@ represents the least upper bound of an @Optic k@ and an @Optic
-- l@. This means in particular that composition of an @Optic k@ and an @Optic
-- k@ will yield an @Optic (Join k l)@.
--
type family Join (k :: OpticKind) (l :: OpticKind) where
  -- BEGIN GENERATED CONTENT
 
  -- An_Iso-----
  Join An_Iso             A_ReversedLens     = A_ReversedLens
  Join An_Iso             A_ReversedPrism    = A_ReversedPrism
  Join An_Iso             A_Prism            = A_Prism
  Join An_Iso             A_Review           = A_Review
  Join An_Iso             A_Lens             = A_Lens
  Join An_Iso             A_Getter           = A_Getter
  Join An_Iso             A_Traversal1       = A_Traversal1
  Join An_Iso             A_Fold1            = A_Fold1
  Join An_Iso             An_AffineTraversal = An_AffineTraversal
  Join An_Iso             An_AffineFold      = An_AffineFold
  Join An_Iso             A_Traversal        = A_Traversal
  Join An_Iso             A_Fold             = A_Fold
  Join An_Iso             A_Setter           = A_Setter

  -- A_ReversedLens-----
  Join A_ReversedLens     An_Iso             = A_ReversedLens
  -- no Join with         A_ReversedPrism
  Join A_ReversedLens     A_Prism            = A_Review
  Join A_ReversedLens     A_Review           = A_Review
  -- no Join with         A_Lens
  -- no Join with         A_Getter
  -- no Join with         A_Traversal1
  -- no Join with         A_Fold1
  -- no Join with         An_AffineTraversal
  -- no Join with         An_AffineFold
  -- no Join with         A_Traversal
  -- no Join with         A_Fold
  -- no Join with         A_Setter

  -- A_ReversedPrism-----
  Join A_ReversedPrism    An_Iso             = A_ReversedPrism
  -- no Join with         A_ReversedLens
  Join A_ReversedPrism    A_Prism            = An_AffineFold
  -- no Join with         A_Review
  Join A_ReversedPrism    A_Lens             = A_Getter
  Join A_ReversedPrism    A_Getter           = A_Getter
  Join A_ReversedPrism    A_Traversal1       = A_Fold1
  Join A_ReversedPrism    A_Fold1            = A_Fold1
  Join A_ReversedPrism    An_AffineTraversal = An_AffineFold
  Join A_ReversedPrism    An_AffineFold      = An_AffineFold
  Join A_ReversedPrism    A_Traversal        = A_Fold
  Join A_ReversedPrism    A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- A_Prism-----
  Join A_Prism            An_Iso             = A_Prism
  Join A_Prism            A_ReversedLens     = A_Review
  Join A_Prism            A_ReversedPrism    = An_AffineFold
  Join A_Prism            A_Review           = A_Review
  Join A_Prism            A_Lens             = An_AffineTraversal
  Join A_Prism            A_Getter           = An_AffineFold
  Join A_Prism            A_Traversal1       = A_Traversal
  Join A_Prism            A_Fold1            = A_Fold
  Join A_Prism            An_AffineTraversal = An_AffineTraversal
  Join A_Prism            An_AffineFold      = An_AffineFold
  Join A_Prism            A_Traversal        = A_Traversal
  Join A_Prism            A_Fold             = A_Fold
  Join A_Prism            A_Setter           = A_Setter

  -- A_Review-----
  Join A_Review           An_Iso             = A_Review
  Join A_Review           A_ReversedLens     = A_Review
  -- no Join with         A_ReversedPrism
  Join A_Review           A_Prism            = A_Review
  -- no Join with         A_Lens
  -- no Join with         A_Getter
  -- no Join with         A_Traversal1
  -- no Join with         A_Fold1
  -- no Join with         An_AffineTraversal
  -- no Join with         An_AffineFold
  -- no Join with         A_Traversal
  -- no Join with         A_Fold
  -- no Join with         A_Setter

  -- A_Lens-----
  Join A_Lens             An_Iso             = A_Lens
  -- no Join with         A_ReversedLens
  Join A_Lens             A_ReversedPrism    = A_Getter
  Join A_Lens             A_Prism            = An_AffineTraversal
  -- no Join with         A_Review
  Join A_Lens             A_Getter           = A_Getter
  Join A_Lens             A_Traversal1       = A_Traversal1
  Join A_Lens             A_Fold1            = A_Fold1
  Join A_Lens             An_AffineTraversal = An_AffineTraversal
  Join A_Lens             An_AffineFold      = An_AffineFold
  Join A_Lens             A_Traversal        = A_Traversal
  Join A_Lens             A_Fold             = A_Fold
  Join A_Lens             A_Setter           = A_Setter

  -- A_Getter-----
  Join A_Getter           An_Iso             = A_Getter
  -- no Join with         A_ReversedLens
  Join A_Getter           A_ReversedPrism    = A_Getter
  Join A_Getter           A_Prism            = An_AffineFold
  -- no Join with         A_Review
  Join A_Getter           A_Lens             = A_Getter
  Join A_Getter           A_Traversal1       = A_Fold1
  Join A_Getter           A_Fold1            = A_Fold1
  Join A_Getter           An_AffineTraversal = An_AffineFold
  Join A_Getter           An_AffineFold      = An_AffineFold
  Join A_Getter           A_Traversal        = A_Fold
  Join A_Getter           A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- A_Traversal1-----
  Join A_Traversal1       An_Iso             = A_Traversal1
  -- no Join with         A_ReversedLens
  Join A_Traversal1       A_ReversedPrism    = A_Fold1
  Join A_Traversal1       A_Prism            = A_Traversal
  -- no Join with         A_Review
  Join A_Traversal1       A_Lens             = A_Traversal1
  Join A_Traversal1       A_Getter           = A_Fold1
  Join A_Traversal1       A_Fold1            = A_Fold1
  Join A_Traversal1       An_AffineTraversal = A_Traversal
  Join A_Traversal1       An_AffineFold      = A_Fold
  Join A_Traversal1       A_Traversal        = A_Traversal
  Join A_Traversal1       A_Fold             = A_Fold
  Join A_Traversal1       A_Setter           = A_Setter

  -- A_Fold1-----
  Join A_Fold1            An_Iso             = A_Fold1
  -- no Join with         A_ReversedLens
  Join A_Fold1            A_ReversedPrism    = A_Fold1
  Join A_Fold1            A_Prism            = A_Fold
  -- no Join with         A_Review
  Join A_Fold1            A_Lens             = A_Fold1
  Join A_Fold1            A_Getter           = A_Fold1
  Join A_Fold1            A_Traversal1       = A_Fold1
  Join A_Fold1            An_AffineTraversal = A_Fold
  Join A_Fold1            An_AffineFold      = A_Fold
  Join A_Fold1            A_Traversal        = A_Fold
  Join A_Fold1            A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- An_AffineTraversal-----
  Join An_AffineTraversal An_Iso             = An_AffineTraversal
  -- no Join with         A_ReversedLens
  Join An_AffineTraversal A_ReversedPrism    = An_AffineFold
  Join An_AffineTraversal A_Prism            = An_AffineTraversal
  -- no Join with         A_Review
  Join An_AffineTraversal A_Lens             = An_AffineTraversal
  Join An_AffineTraversal A_Getter           = An_AffineFold
  Join An_AffineTraversal A_Traversal1       = A_Traversal
  Join An_AffineTraversal A_Fold1            = A_Fold
  Join An_AffineTraversal An_AffineFold      = An_AffineFold
  Join An_AffineTraversal A_Traversal        = A_Traversal
  Join An_AffineTraversal A_Fold             = A_Fold
  Join An_AffineTraversal A_Setter           = A_Setter

  -- An_AffineFold-----
  Join An_AffineFold      An_Iso             = An_AffineFold
  -- no Join with         A_ReversedLens
  Join An_AffineFold      A_ReversedPrism    = An_AffineFold
  Join An_AffineFold      A_Prism            = An_AffineFold
  -- no Join with         A_Review
  Join An_AffineFold      A_Lens             = An_AffineFold
  Join An_AffineFold      A_Getter           = An_AffineFold
  Join An_AffineFold      A_Traversal1       = A_Fold
  Join An_AffineFold      A_Fold1            = A_Fold
  Join An_AffineFold      An_AffineTraversal = An_AffineFold
  Join An_AffineFold      A_Traversal        = A_Fold
  Join An_AffineFold      A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- A_Traversal-----
  Join A_Traversal        An_Iso             = A_Traversal
  -- no Join with         A_ReversedLens
  Join A_Traversal        A_ReversedPrism    = A_Fold
  Join A_Traversal        A_Prism            = A_Traversal
  -- no Join with         A_Review
  Join A_Traversal        A_Lens             = A_Traversal
  Join A_Traversal        A_Getter           = A_Fold
  Join A_Traversal        A_Traversal1       = A_Traversal
  Join A_Traversal        A_Fold1            = A_Fold
  Join A_Traversal        An_AffineTraversal = A_Traversal
  Join A_Traversal        An_AffineFold      = A_Fold
  Join A_Traversal        A_Fold             = A_Fold
  Join A_Traversal        A_Setter           = A_Setter

  -- A_Fold-----
  Join A_Fold             An_Iso             = A_Fold
  -- no Join with         A_ReversedLens
  Join A_Fold             A_ReversedPrism    = A_Fold
  Join A_Fold             A_Prism            = A_Fold
  -- no Join with         A_Review
  Join A_Fold             A_Lens             = A_Fold
  Join A_Fold             A_Getter           = A_Fold
  Join A_Fold             A_Traversal1       = A_Fold
  Join A_Fold             A_Fold1            = A_Fold
  Join A_Fold             An_AffineTraversal = A_Fold
  Join A_Fold             An_AffineFold      = A_Fold
  Join A_Fold             A_Traversal        = A_Fold
  -- no Join with         A_Setter

  -- A_Setter-----
  Join A_Setter           An_Iso             = A_Setter
  -- no Join with         A_ReversedLens
  -- no Join with         A_ReversedPrism
  Join A_Setter           A_Prism            = A_Setter
  -- no Join with         A_Review
  Join A_Setter           A_Lens             = A_Setter
  -- no Join with         A_Getter
  Join A_Setter           A_Traversal1       = A_Setter
  -- no Join with         A_Fold1
  Join A_Setter           An_AffineTraversal = A_Setter
  -- no Join with         An_AffineFold
  Join A_Setter           A_Traversal        = A_Setter
  -- no Join with         A_Fold

  -- END GENERATED CONTENT

  -- Every optic kinds can be joined with itself.
  Join k k = k

  -- Everything else is a type error.
  Join k l = TypeError ('ShowType k
                        ':<>: 'Text " cannot be composed with "
                        ':<>: 'ShowType l)
