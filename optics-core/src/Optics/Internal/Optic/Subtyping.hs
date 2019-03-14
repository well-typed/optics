{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Instances to implement the subtyping hierarchy between optics.
--
module Optics.Internal.Optic.Subtyping where

import GHC.TypeLits (ErrorMessage(..), TypeError)

import Optics.Internal.Optic.Types

-- | Subtyping relationship between flavours of optics.
--
-- An instance of @Is k l@ represents that any @Optic k@ can be used as an
-- @Optic l@. For example, we have an @Is A_Lens A_Traversal@ instance, but not
-- @Is A_Traversal A_Lens@.
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

-- | Every flavour of optic can be used as itself.
instance Is k k where
  implies _ = id

----------------------------------------

-- BEGIN GENERATED CONTENT

-- An_Equality
instance Is An_Equality        An_Iso             where implies _ = id
instance Is An_Equality        A_LensyReview      where implies _ = id
instance Is An_Equality        A_PrismaticGetter  where implies _ = id
instance Is An_Equality        A_Prism            where implies _ = id
instance Is An_Equality        A_Review           where implies _ = id
instance Is An_Equality        A_Lens             where implies _ = id
instance Is An_Equality        A_Getter           where implies _ = id
instance Is An_Equality        An_AffineTraversal where implies _ = id
instance Is An_Equality        An_AffineFold      where implies _ = id
instance Is An_Equality        A_Traversal        where implies _ = id
instance Is An_Equality        A_Fold             where implies _ = id
instance Is An_Equality        A_Setter           where implies _ = id
-- An_Iso
instance Is An_Iso             A_LensyReview      where implies _ = id
instance Is An_Iso             A_PrismaticGetter  where implies _ = id
instance Is An_Iso             A_Prism            where implies _ = id
instance Is An_Iso             A_Review           where implies _ = id
instance Is An_Iso             A_Lens             where implies _ = id
instance Is An_Iso             A_Getter           where implies _ = id
instance Is An_Iso             An_AffineTraversal where implies _ = id
instance Is An_Iso             An_AffineFold      where implies _ = id
instance Is An_Iso             A_Traversal        where implies _ = id
instance Is An_Iso             A_Fold             where implies _ = id
instance Is An_Iso             A_Setter           where implies _ = id
-- A_LensyReview
instance Is A_LensyReview      A_Review           where implies _ = id
-- A_PrismaticGetter
instance Is A_PrismaticGetter  A_Getter           where implies _ = id
instance Is A_PrismaticGetter  An_AffineFold      where implies _ = id
instance Is A_PrismaticGetter  A_Fold             where implies _ = id
-- A_Prism
instance Is A_Prism            A_Review           where implies _ = id
instance Is A_Prism            An_AffineTraversal where implies _ = id
instance Is A_Prism            An_AffineFold      where implies _ = id
instance Is A_Prism            A_Traversal        where implies _ = id
instance Is A_Prism            A_Fold             where implies _ = id
instance Is A_Prism            A_Setter           where implies _ = id
-- A_Lens
instance Is A_Lens             A_Getter           where implies _ = id
instance Is A_Lens             An_AffineTraversal where implies _ = id
instance Is A_Lens             An_AffineFold      where implies _ = id
instance Is A_Lens             A_Traversal        where implies _ = id
instance Is A_Lens             A_Fold             where implies _ = id
instance Is A_Lens             A_Setter           where implies _ = id
-- A_Getter
instance Is A_Getter           An_AffineFold      where implies _ = id
instance Is A_Getter           A_Fold             where implies _ = id
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

-- | Computes the least upper bound of two optics flavours.
--
-- @Join k l@ represents the least upper bound of an @Optic k@ and an @Optic
-- l@. This means in particular that composition of an @Optic k@ and an @Optic
-- k@ will yield an @Optic (Join k l)@.
--
type family Join (k :: *) (l :: *) where
  -- BEGIN GENERATED CONTENT
  -- An_Equality-----
  Join An_Equality        An_Iso             = An_Iso
  Join An_Equality        A_LensyReview      = A_LensyReview
  Join An_Equality        A_PrismaticGetter  = A_PrismaticGetter
  Join An_Equality        A_Prism            = A_Prism
  Join An_Equality        A_Review           = A_Review
  Join An_Equality        A_Lens             = A_Lens
  Join An_Equality        A_Getter           = A_Getter
  Join An_Equality        An_AffineTraversal = An_AffineTraversal
  Join An_Equality        An_AffineFold      = An_AffineFold
  Join An_Equality        A_Traversal        = A_Traversal
  Join An_Equality        A_Fold             = A_Fold
  Join An_Equality        A_Setter           = A_Setter

  -- An_Iso-----
  Join An_Iso             An_Equality        = An_Iso
  Join An_Iso             A_LensyReview      = A_LensyReview
  Join An_Iso             A_PrismaticGetter  = A_PrismaticGetter
  Join An_Iso             A_Prism            = A_Prism
  Join An_Iso             A_Review           = A_Review
  Join An_Iso             A_Lens             = A_Lens
  Join An_Iso             A_Getter           = A_Getter
  Join An_Iso             An_AffineTraversal = An_AffineTraversal
  Join An_Iso             An_AffineFold      = An_AffineFold
  Join An_Iso             A_Traversal        = A_Traversal
  Join An_Iso             A_Fold             = A_Fold
  Join An_Iso             A_Setter           = A_Setter

  -- A_LensyReview-----
  Join A_LensyReview      An_Equality        = A_LensyReview
  Join A_LensyReview      An_Iso             = A_LensyReview
  -- no Join with         A_PrismaticGetter
  Join A_LensyReview      A_Prism            = A_Review
  Join A_LensyReview      A_Review           = A_Review
  -- no Join with         A_Lens
  -- no Join with         A_Getter
  -- no Join with         An_AffineTraversal
  -- no Join with         An_AffineFold
  -- no Join with         A_Traversal
  -- no Join with         A_Fold
  -- no Join with         A_Setter

  -- A_PrismaticGetter-----
  Join A_PrismaticGetter  An_Equality        = A_PrismaticGetter
  Join A_PrismaticGetter  An_Iso             = A_PrismaticGetter
  -- no Join with         A_LensyReview
  Join A_PrismaticGetter  A_Prism            = An_AffineFold
  -- no Join with         A_Review
  Join A_PrismaticGetter  A_Lens             = A_Getter
  Join A_PrismaticGetter  A_Getter           = A_Getter
  Join A_PrismaticGetter  An_AffineTraversal = An_AffineFold
  Join A_PrismaticGetter  An_AffineFold      = An_AffineFold
  Join A_PrismaticGetter  A_Traversal        = A_Fold
  Join A_PrismaticGetter  A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- A_Prism-----
  Join A_Prism            An_Equality        = A_Prism
  Join A_Prism            An_Iso             = A_Prism
  Join A_Prism            A_LensyReview      = A_Review
  Join A_Prism            A_PrismaticGetter  = An_AffineFold
  Join A_Prism            A_Review           = A_Review
  Join A_Prism            A_Lens             = An_AffineTraversal
  Join A_Prism            A_Getter           = An_AffineFold
  Join A_Prism            An_AffineTraversal = An_AffineTraversal
  Join A_Prism            An_AffineFold      = An_AffineFold
  Join A_Prism            A_Traversal        = A_Traversal
  Join A_Prism            A_Fold             = A_Fold
  Join A_Prism            A_Setter           = A_Setter

  -- A_Review-----
  Join A_Review           An_Equality        = A_Review
  Join A_Review           An_Iso             = A_Review
  Join A_Review           A_LensyReview      = A_Review
  -- no Join with         A_PrismaticGetter
  Join A_Review           A_Prism            = A_Review
  -- no Join with         A_Lens
  -- no Join with         A_Getter
  -- no Join with         An_AffineTraversal
  -- no Join with         An_AffineFold
  -- no Join with         A_Traversal
  -- no Join with         A_Fold
  -- no Join with         A_Setter

  -- A_Lens-----
  Join A_Lens             An_Equality        = A_Lens
  Join A_Lens             An_Iso             = A_Lens
  -- no Join with         A_LensyReview
  Join A_Lens             A_PrismaticGetter  = A_Getter
  Join A_Lens             A_Prism            = An_AffineTraversal
  -- no Join with         A_Review
  Join A_Lens             A_Getter           = A_Getter
  Join A_Lens             An_AffineTraversal = An_AffineTraversal
  Join A_Lens             An_AffineFold      = An_AffineFold
  Join A_Lens             A_Traversal        = A_Traversal
  Join A_Lens             A_Fold             = A_Fold
  Join A_Lens             A_Setter           = A_Setter

  -- A_Getter-----
  Join A_Getter           An_Equality        = A_Getter
  Join A_Getter           An_Iso             = A_Getter
  -- no Join with         A_LensyReview
  Join A_Getter           A_PrismaticGetter  = A_Getter
  Join A_Getter           A_Prism            = An_AffineFold
  -- no Join with         A_Review
  Join A_Getter           A_Lens             = A_Getter
  Join A_Getter           An_AffineTraversal = An_AffineFold
  Join A_Getter           An_AffineFold      = An_AffineFold
  Join A_Getter           A_Traversal        = A_Fold
  Join A_Getter           A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- An_AffineTraversal-----
  Join An_AffineTraversal An_Equality        = An_AffineTraversal
  Join An_AffineTraversal An_Iso             = An_AffineTraversal
  -- no Join with         A_LensyReview
  Join An_AffineTraversal A_PrismaticGetter  = An_AffineFold
  Join An_AffineTraversal A_Prism            = An_AffineTraversal
  -- no Join with         A_Review
  Join An_AffineTraversal A_Lens             = An_AffineTraversal
  Join An_AffineTraversal A_Getter           = An_AffineFold
  Join An_AffineTraversal An_AffineFold      = An_AffineFold
  Join An_AffineTraversal A_Traversal        = A_Traversal
  Join An_AffineTraversal A_Fold             = A_Fold
  Join An_AffineTraversal A_Setter           = A_Setter

  -- An_AffineFold-----
  Join An_AffineFold      An_Equality        = An_AffineFold
  Join An_AffineFold      An_Iso             = An_AffineFold
  -- no Join with         A_LensyReview
  Join An_AffineFold      A_PrismaticGetter  = An_AffineFold
  Join An_AffineFold      A_Prism            = An_AffineFold
  -- no Join with         A_Review
  Join An_AffineFold      A_Lens             = An_AffineFold
  Join An_AffineFold      A_Getter           = An_AffineFold
  Join An_AffineFold      An_AffineTraversal = An_AffineFold
  Join An_AffineFold      A_Traversal        = A_Fold
  Join An_AffineFold      A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- A_Traversal-----
  Join A_Traversal        An_Equality        = A_Traversal
  Join A_Traversal        An_Iso             = A_Traversal
  -- no Join with         A_LensyReview
  Join A_Traversal        A_PrismaticGetter  = A_Fold
  Join A_Traversal        A_Prism            = A_Traversal
  -- no Join with         A_Review
  Join A_Traversal        A_Lens             = A_Traversal
  Join A_Traversal        A_Getter           = A_Fold
  Join A_Traversal        An_AffineTraversal = A_Traversal
  Join A_Traversal        An_AffineFold      = A_Fold
  Join A_Traversal        A_Fold             = A_Fold
  Join A_Traversal        A_Setter           = A_Setter

  -- A_Fold-----
  Join A_Fold             An_Equality        = A_Fold
  Join A_Fold             An_Iso             = A_Fold
  -- no Join with         A_LensyReview
  Join A_Fold             A_PrismaticGetter  = A_Fold
  Join A_Fold             A_Prism            = A_Fold
  -- no Join with         A_Review
  Join A_Fold             A_Lens             = A_Fold
  Join A_Fold             A_Getter           = A_Fold
  Join A_Fold             An_AffineTraversal = A_Fold
  Join A_Fold             An_AffineFold      = A_Fold
  Join A_Fold             A_Traversal        = A_Fold
  -- no Join with         A_Setter

  -- A_Setter-----
  Join A_Setter           An_Equality        = A_Setter
  Join A_Setter           An_Iso             = A_Setter
  -- no Join with         A_LensyReview
  -- no Join with         A_PrismaticGetter
  Join A_Setter           A_Prism            = A_Setter
  -- no Join with         A_Review
  Join A_Setter           A_Lens             = A_Setter
  -- no Join with         A_Getter
  Join A_Setter           An_AffineTraversal = A_Setter
  -- no Join with         An_AffineFold
  Join A_Setter           A_Traversal        = A_Setter
  -- no Join with         A_Fold

  -- END GENERATED CONTENT

  -- Every optics flavour can be joined with itself.
  Join k k = k

  -- Everything else is a type error.
  Join k l = TypeError ('ShowType k
                        ':<>: 'Text " cannot be composed with "
                        ':<>: 'ShowType l)
