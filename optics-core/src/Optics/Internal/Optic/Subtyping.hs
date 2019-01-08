{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instances to implement the subtyping hierarchy between optics.
--
module Optics.Internal.Optic.Subtyping where

import GHC.TypeLits (ErrorMessage(..), TypeError)

import Optics.Internal.Optic.Types

-- | Subtyping relationship between flavours of optics.
--
-- An instance of @Is k l@ represents that any @Optic k@ can be used as an
-- @Optic l@. For example, we have an @Is 'A_Lens 'A_Traversal@ instance, but not
-- @Is 'A_Traversal 'A_Lens@.
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

-- Instances for 'An_IxFold
--
-- (none)

-- Instances for 'A_Fold

instance Is 'A_Fold 'An_IxFold where
  implies _ = id

-- Instances for 'An_AffineFold

instance Is 'An_AffineFold 'An_IxFold where
  implies _ = id

instance Is 'An_AffineFold 'A_Fold where
  implies _ = id

-- Instances for 'An_IxSetter
--
-- (none)

-- Instances for 'A_Setter

instance Is 'A_Setter 'An_IxSetter where
  implies _ = id

-- Instances for 'A_Getter

instance Is 'A_Getter 'An_IxFold where
  implies _ = id

instance Is 'A_Getter 'A_Fold where
  implies _ = id

instance Is 'A_Getter 'An_AffineFold where
  implies _ = id

-- Instances for 'An_IxTraversal

instance Is 'An_IxTraversal 'An_IxFold where
  implies _ = id

instance Is 'An_IxTraversal 'An_IxSetter where
  implies _ = id

-- Instances for 'A_Traversal

instance Is 'A_Traversal 'An_IxFold where
  implies _ = id

instance Is 'A_Traversal 'A_Fold where
  implies _ = id

instance Is 'A_Traversal 'An_IxSetter where
  implies _ = id

instance Is 'A_Traversal 'A_Setter where
  implies _ = id

instance Is 'A_Traversal 'An_IxTraversal where
  implies _ = id

-- Instances for 'An_AffineTraversal

instance Is 'An_AffineTraversal 'An_IxFold where
  implies _ = id

instance Is 'An_AffineTraversal 'A_Fold where
  implies _ = id

instance Is 'An_AffineTraversal 'An_AffineFold where
  implies _ = id

instance Is 'An_AffineTraversal 'An_IxSetter where
  implies _ = id

instance Is 'An_AffineTraversal 'A_Setter where
  implies _ = id

instance Is 'An_AffineTraversal 'An_IxTraversal where
  implies _ = id

instance Is 'An_AffineTraversal 'A_Traversal where
  implies _ = id

-- Instances for 'A_Lens

instance Is 'A_Lens 'An_IxFold where
  implies _ = id

instance Is 'A_Lens 'A_Fold where
  implies _ = id

instance Is 'A_Lens 'An_AffineFold where
  implies _ = id

instance Is 'A_Lens 'An_IxSetter where
  implies _ = id

instance Is 'A_Lens 'A_Setter where
  implies _ = id

instance Is 'A_Lens 'A_Getter where
  implies _ = id

instance Is 'A_Lens 'An_IxTraversal where
  implies _ = id

instance Is 'A_Lens 'A_Traversal where
  implies _ = id

instance Is 'A_Lens 'An_AffineTraversal where
  implies _ = id

-- Instances for 'A_Review
--
-- (none)

-- Instances for 'A_LensyReview

instance Is 'A_LensyReview 'A_Review where
  implies _ = id

-- Instances for 'A_PrismaticGetter

instance Is 'A_PrismaticGetter 'An_IxFold where
  implies _ = id

instance Is 'A_PrismaticGetter 'A_Fold where
  implies _ = id

instance Is 'A_PrismaticGetter 'An_AffineFold where
  implies _ = id

instance Is 'A_PrismaticGetter 'A_Getter where
  implies _ = id

-- Instances for 'A_Prism

instance Is 'A_Prism 'An_IxFold where
  implies _ = id

instance Is 'A_Prism 'A_Fold where
  implies _ = id

instance Is 'A_Prism 'An_AffineFold where
  implies _ = id

instance Is 'A_Prism 'An_IxSetter where
  implies _ = id

instance Is 'A_Prism 'A_Setter where
  implies _ = id

instance Is 'A_Prism 'A_Review where
  implies _ = id

instance Is 'A_Prism 'An_IxTraversal where
  implies _ = id

instance Is 'A_Prism 'A_Traversal where
  implies _ = id

instance Is 'A_Prism 'An_AffineTraversal where
  implies _ = id

-- Instances for 'An_Iso

instance Is 'An_Iso 'An_IxFold where
  implies _ = id

instance Is 'An_Iso 'A_Fold where
  implies _ = id

instance Is 'An_Iso 'An_AffineFold where
  implies _ = id

instance Is 'An_Iso 'An_IxSetter where
  implies _ = id

instance Is 'An_Iso 'A_Setter where
  implies _ = id

instance Is 'An_Iso 'A_Getter where
  implies _ = id

instance Is 'An_Iso 'An_AffineTraversal where
  implies _ = id

instance Is 'An_Iso 'A_Traversal where
  implies _ = id

instance Is 'An_Iso 'An_IxTraversal where
  implies _ = id

instance Is 'An_Iso 'A_Lens where
  implies _ = id

instance Is 'An_Iso 'A_LensyReview where
  implies _ = id

instance Is 'An_Iso 'A_Review where
  implies _ = id

instance Is 'An_Iso 'A_PrismaticGetter where
  implies _ = id

instance Is 'An_Iso 'A_Prism where
  implies _ = id

-- Instances for 'An_Equality

instance Is 'An_Equality 'An_IxFold where
  implies _ = id

instance Is 'An_Equality 'A_Fold where
  implies _ = id

instance Is 'An_Equality 'An_AffineFold where
  implies _ = id

instance Is 'An_Equality 'An_IxSetter where
  implies _ = id

instance Is 'An_Equality 'A_Setter where
  implies _ = id

instance Is 'An_Equality 'A_Getter where
  implies _ = id

instance Is 'An_Equality 'An_AffineTraversal where
  implies _ = id

instance Is 'An_Equality 'A_Traversal where
  implies _ = id

instance Is 'An_Equality 'An_IxTraversal where
  implies _ = id

instance Is 'An_Equality 'A_Lens where
  implies _ = id

instance Is 'An_Equality 'A_LensyReview where
  implies _ = id

instance Is 'An_Equality 'A_Review where
  implies _ = id

instance Is 'An_Equality 'A_PrismaticGetter where
  implies _ = id

instance Is 'An_Equality 'A_Prism where
  implies _ = id

instance Is 'An_Equality 'An_Iso where
  implies _ = id

----------------------------------------

-- | Computes the least upper bound of two optics flavours.
--
-- @Join k l@ represents the least upper bound of an @Optic k@ and an @Optic
-- l@. This means in particular that composition of an @Optic k@ and an @Optic
-- k@ will yield an @Optic (Join k l)@.
--
type family Join (k :: OpticKind) (l :: OpticKind) :: OpticKind where
  Join 'An_IxFold          'A_Fold               = 'An_IxFold
  Join 'An_IxFold          'An_AffineFold        = 'An_IxFold
  Join 'An_IxFold          'A_Getter             = 'An_IxFold
  Join 'An_IxFold          'A_PrismaticGetter    = 'An_IxFold
  Join 'An_IxFold          'An_IxTraversal       = 'An_IxFold
  Join 'An_IxFold          'A_Traversal          = 'An_IxFold
  Join 'An_IxFold          'An_AffineTraversal   = 'An_IxFold
  Join 'An_IxFold          'A_Lens               = 'An_IxFold
  Join 'An_IxFold          'A_Prism              = 'An_IxFold
  Join 'An_IxFold          'An_Iso               = 'An_IxFold
  Join 'An_IxFold          'An_Equality          = 'An_IxFold
  -- Doesn't join with:
  -- - IxSetter
  -- - Setter
  -- - LensyReview
  -- - Review

  Join 'A_Fold             'An_IxFold            = 'An_IxFold
  Join 'A_Fold             'An_AffineFold        = 'A_Fold
  Join 'A_Fold             'A_Getter             = 'A_Fold
  Join 'A_Fold             'A_PrismaticGetter    = 'A_Fold
  Join 'A_Fold             'An_IxTraversal       = 'An_IxFold
  Join 'A_Fold             'A_Traversal          = 'A_Fold
  Join 'A_Fold             'An_AffineTraversal   = 'A_Fold
  Join 'A_Fold             'A_Lens               = 'A_Fold
  Join 'A_Fold             'A_Prism              = 'A_Fold
  Join 'A_Fold             'An_Iso               = 'A_Fold
  Join 'A_Fold             'An_Equality          = 'A_Fold
  -- Doesn't join with:
  -- - IxSetter
  -- - Setter
  -- - LensyReview
  -- - Review

  Join 'An_AffineFold      'An_IxFold            = 'An_IxFold
  Join 'An_AffineFold      'A_Fold               = 'A_Fold
  Join 'An_AffineFold      'A_Getter             = 'An_AffineFold
  Join 'An_AffineFold      'A_PrismaticGetter    = 'An_AffineFold
  Join 'An_AffineFold      'An_IxTraversal       = 'An_IxFold
  Join 'An_AffineFold      'A_Traversal          = 'A_Fold
  Join 'An_AffineFold      'An_AffineTraversal   = 'An_AffineFold
  Join 'An_AffineFold      'A_Lens               = 'An_AffineFold
  Join 'An_AffineFold      'A_Prism              = 'An_AffineFold
  Join 'An_AffineFold      'An_Iso               = 'An_AffineFold
  Join 'An_AffineFold      'An_Equality          = 'An_AffineFold
  -- Doesn't join with:
  -- - IxSetter
  -- - Setter
  -- - LensyReview
  -- - Review

  Join 'An_IxSetter        'A_Setter             = 'An_IxSetter
  Join 'An_IxSetter        'An_IxTraversal       = 'An_IxSetter
  Join 'An_IxSetter        'A_Traversal          = 'An_IxSetter
  Join 'An_IxSetter        'An_AffineTraversal   = 'An_IxSetter
  Join 'An_IxSetter        'A_Lens               = 'An_IxSetter
  Join 'An_IxSetter        'A_Prism              = 'An_IxSetter
  Join 'An_IxSetter        'An_Iso               = 'An_IxSetter
  Join 'An_IxSetter        'An_Equality          = 'An_IxSetter
  -- Doesn't join with:
  -- - IxFold
  -- - Fold
  -- - AffineFold
  -- - PrismaticGetter
  -- - Getter
  -- - LensyReview
  -- - Review

  Join 'A_Setter           'An_IxSetter          = 'An_IxSetter
  Join 'A_Setter           'An_IxTraversal       = 'An_IxSetter
  Join 'A_Setter           'A_Traversal          = 'A_Setter
  Join 'A_Setter           'An_AffineTraversal   = 'A_Setter
  Join 'A_Setter           'A_Lens               = 'A_Setter
  Join 'A_Setter           'A_Prism              = 'A_Setter
  Join 'A_Setter           'An_Iso               = 'A_Setter
  Join 'A_Setter           'An_Equality          = 'A_Setter
  -- Doesn't join with:
  -- - IxFold
  -- - Fold
  -- - AffineFold
  -- - PrismaticGetter
  -- - Getter
  -- - LensyReview
  -- - Review

  Join 'A_Getter           'An_IxFold            = 'An_IxFold
  Join 'A_Getter           'A_Fold               = 'A_Fold
  Join 'A_Getter           'An_AffineFold        = 'An_AffineFold
  Join 'A_Getter           'An_IxTraversal       = 'An_IxFold
  Join 'A_Getter           'A_Traversal          = 'A_Fold
  Join 'A_Getter           'An_AffineTraversal   = 'An_AffineFold
  Join 'A_Getter           'A_Lens               = 'A_Getter
  Join 'A_Getter           'A_PrismaticGetter    = 'A_Getter
  Join 'A_Getter           'A_Prism              = 'An_AffineFold
  Join 'A_Getter           'An_Iso               = 'A_Getter
  Join 'A_Getter           'An_Equality          = 'A_Getter
  -- Doesn't join with:
  -- - IxSetter
  -- - Setter
  -- - LensyReview
  -- - Review

  Join 'A_PrismaticGetter  'An_IxFold            = 'An_IxFold
  Join 'A_PrismaticGetter  'A_Fold               = 'A_Fold
  Join 'A_PrismaticGetter  'An_AffineFold        = 'An_AffineFold
  Join 'A_PrismaticGetter  'An_IxTraversal       = 'An_IxFold
  Join 'A_PrismaticGetter  'A_Traversal          = 'A_Fold
  Join 'A_PrismaticGetter  'An_AffineTraversal   = 'An_AffineFold
  Join 'A_PrismaticGetter  'A_Lens               = 'A_Getter
  Join 'A_PrismaticGetter  'A_Getter             = 'A_Getter
  Join 'A_PrismaticGetter  'A_Prism              = 'An_AffineFold
  Join 'A_PrismaticGetter  'An_Iso               = 'A_PrismaticGetter
  Join 'A_PrismaticGetter  'An_Equality          = 'A_PrismaticGetter
  -- Doesn't join with:
  -- - IxSetter
  -- - Setter
  -- - LensyReview
  -- - Review

  Join 'An_IxTraversal     'An_IxFold            = 'An_IxFold
  Join 'An_IxTraversal     'A_Fold               = 'An_IxFold
  Join 'An_IxTraversal     'An_AffineFold        = 'An_IxFold
  Join 'An_IxTraversal     'An_IxSetter          = 'An_IxSetter
  Join 'An_IxTraversal     'A_Setter             = 'An_IxSetter
  Join 'An_IxTraversal     'A_Getter             = 'An_IxFold
  Join 'An_IxTraversal     'A_PrismaticGetter    = 'An_IxFold
  Join 'An_IxTraversal     'A_Traversal          = 'An_IxTraversal
  Join 'An_IxTraversal     'An_AffineTraversal   = 'An_IxTraversal
  Join 'An_IxTraversal     'A_Lens               = 'An_IxTraversal
  Join 'An_IxTraversal     'A_Prism              = 'An_IxTraversal
  Join 'An_IxTraversal     'An_Iso               = 'An_IxTraversal
  Join 'An_IxTraversal     'An_Equality          = 'An_IxTraversal
  -- Doesn't join with:
  -- - LensyReview
  -- - Review

  Join 'A_Traversal        'An_IxFold            = 'An_IxFold
  Join 'A_Traversal        'A_Fold               = 'A_Fold
  Join 'A_Traversal        'An_AffineFold        = 'A_Fold
  Join 'A_Traversal        'An_IxSetter          = 'An_IxSetter
  Join 'A_Traversal        'A_Setter             = 'A_Setter
  Join 'A_Traversal        'A_Getter             = 'A_Fold
  Join 'A_Traversal        'A_PrismaticGetter    = 'A_Fold
  Join 'A_Traversal        'An_IxTraversal       = 'An_IxTraversal
  Join 'A_Traversal        'An_AffineTraversal   = 'A_Traversal
  Join 'A_Traversal        'A_Lens               = 'A_Traversal
  Join 'A_Traversal        'A_Prism              = 'A_Traversal
  Join 'A_Traversal        'An_Iso               = 'A_Traversal
  Join 'A_Traversal        'An_Equality          = 'A_Traversal
  -- Doesn't join with:
  -- - LensyReview
  -- - Review

  Join 'An_AffineTraversal 'An_IxFold            = 'An_IxFold
  Join 'An_AffineTraversal 'A_Fold               = 'A_Fold
  Join 'An_AffineTraversal 'An_AffineFold        = 'An_AffineFold
  Join 'An_AffineTraversal 'An_IxSetter          = 'An_IxSetter
  Join 'An_AffineTraversal 'A_Setter             = 'A_Setter
  Join 'An_AffineTraversal 'A_Getter             = 'An_AffineFold
  Join 'An_AffineTraversal 'A_PrismaticGetter    = 'An_AffineFold
  Join 'An_AffineTraversal 'An_IxTraversal       = 'An_IxTraversal
  Join 'An_AffineTraversal 'A_Traversal          = 'A_Traversal
  Join 'An_AffineTraversal 'A_Lens               = 'An_AffineTraversal
  Join 'An_AffineTraversal 'A_Prism              = 'An_AffineTraversal
  Join 'An_AffineTraversal 'An_Iso               = 'An_AffineTraversal
  Join 'An_AffineTraversal 'An_Equality          = 'An_AffineTraversal
  -- Doesn't join with:
  -- - LensyReview
  -- - Review

  Join 'A_Lens             'An_IxFold            = 'An_IxFold
  Join 'A_Lens             'A_Fold               = 'A_Fold
  Join 'A_Lens             'An_AffineFold        = 'An_AffineFold
  Join 'A_Lens             'An_IxSetter          = 'An_IxSetter
  Join 'A_Lens             'A_Setter             = 'A_Setter
  Join 'A_Lens             'A_Getter             = 'A_Getter
  Join 'A_Lens             'A_PrismaticGetter    = 'A_Getter
  Join 'A_Lens             'An_IxTraversal       = 'An_IxTraversal
  Join 'A_Lens             'A_Traversal          = 'A_Traversal
  Join 'A_Lens             'An_AffineTraversal   = 'An_AffineTraversal
  Join 'A_Lens             'A_Prism              = 'An_AffineTraversal
  Join 'A_Lens             'An_Iso               = 'A_Lens
  Join 'A_Lens             'An_Equality          = 'A_Lens
  -- Doesn't join with:
  -- - LensyReview
  -- - Review

  Join 'A_LensyReview      'A_Review             = 'A_Review
  Join 'A_LensyReview      'A_Prism              = 'A_Review
  Join 'A_LensyReview      'An_Iso               = 'A_LensyReview
  Join 'A_LensyReview      'An_Equality          = 'A_LensyReview
  -- Doesn't join with:
  -- - IxFold
  -- - Fold
  -- - AffineFold
  -- - IxSetter
  -- - Setter
  -- - PrismaticGetter
  -- - Getter
  -- - IxTraversal
  -- - Traversal
  -- - AffineTraversal
  -- - Lens

  Join 'A_Review           'A_LensyReview        = 'A_Review
  Join 'A_Review           'A_Prism              = 'A_Review
  Join 'A_Review           'An_Iso               = 'A_Review
  Join 'A_Review           'An_Equality          = 'A_Review
  -- Doesn't join with:
  -- - IxFold
  -- - Fold
  -- - AffineFold
  -- - IxSetter
  -- - Setter
  -- - PrismaticGetter
  -- - Getter
  -- - IxTraversal
  -- - Traversal
  -- - AffineTraversal
  -- - Lens

  Join 'A_Prism            'An_IxFold            = 'An_IxFold
  Join 'A_Prism            'A_Fold               = 'A_Fold
  Join 'A_Prism            'An_AffineFold        = 'An_AffineFold
  Join 'A_Prism            'An_IxSetter          = 'An_IxSetter
  Join 'A_Prism            'A_Setter             = 'A_Setter
  Join 'A_Prism            'A_Getter             = 'An_AffineFold
  Join 'A_Prism            'A_PrismaticGetter    = 'An_AffineFold
  Join 'A_Prism            'An_IxTraversal       = 'An_IxTraversal
  Join 'A_Prism            'A_Traversal          = 'A_Traversal
  Join 'A_Prism            'An_AffineTraversal   = 'An_AffineTraversal
  Join 'A_Prism            'A_Lens               = 'An_AffineTraversal
  Join 'A_Prism            'A_LensyReview        = 'A_Review
  Join 'A_Prism            'A_Review             = 'A_Review
  Join 'A_Prism            'An_Iso               = 'A_Prism
  Join 'A_Prism            'An_Equality          = 'A_Prism

  Join 'An_Iso             'An_IxFold            = 'An_IxFold
  Join 'An_Iso             'A_Fold               = 'A_Fold
  Join 'An_Iso             'An_AffineFold        = 'An_AffineFold
  Join 'An_Iso             'An_IxSetter          = 'An_IxSetter
  Join 'An_Iso             'A_Setter             = 'A_Setter
  Join 'An_Iso             'A_Getter             = 'A_Getter
  Join 'An_Iso             'A_PrismaticGetter    = 'A_PrismaticGetter
  Join 'An_Iso             'An_IxTraversal       = 'An_IxTraversal
  Join 'An_Iso             'A_Traversal          = 'A_Traversal
  Join 'An_Iso             'An_AffineTraversal   = 'An_AffineTraversal
  Join 'An_Iso             'A_Lens               = 'A_Lens
  Join 'An_Iso             'A_LensyReview        = 'A_LensyReview
  Join 'An_Iso             'A_Review             = 'A_Review
  Join 'An_Iso             'A_Prism              = 'A_Prism
  Join 'An_Iso             'An_Equality          = 'An_Iso

  Join 'An_Equality        'An_IxFold            = 'An_IxFold
  Join 'An_Equality        'A_Fold               = 'A_Fold
  Join 'An_Equality        'An_AffineFold        = 'An_AffineFold
  Join 'An_Equality        'An_IxSetter          = 'An_IxSetter
  Join 'An_Equality        'A_Setter             = 'A_Setter
  Join 'An_Equality        'A_Getter             = 'A_Getter
  Join 'An_Equality        'A_PrismaticGetter    = 'A_PrismaticGetter
  Join 'An_Equality        'An_IxTraversal       = 'An_IxTraversal
  Join 'An_Equality        'A_Traversal          = 'A_Traversal
  Join 'An_Equality        'An_AffineTraversal   = 'An_AffineTraversal
  Join 'An_Equality        'A_Lens               = 'A_Lens
  Join 'An_Equality        'A_LensyReview        = 'A_LensyReview
  Join 'An_Equality        'A_Review             = 'A_Review
  Join 'An_Equality        'A_Prism              = 'A_Prism
  Join 'An_Equality        'An_Iso               = 'An_Iso

  -- Every optics flavour can be joined with itself.
  Join k k = k

  -- Everything else is a type error.
  Join k l = TypeError ('ShowType k
                        ':<>: 'Text " cannot be composed with "
                        ':<>: 'ShowType l)
