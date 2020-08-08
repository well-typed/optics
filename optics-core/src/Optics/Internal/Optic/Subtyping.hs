{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Instances to implement the subtyping hierarchy between optics.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Optic.Subtyping where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Optics.Internal.Optic.TypeLevel
import Optics.Internal.Optic.Types

-- | Subtyping relationship between kinds of optics.
--
-- An instance of @'Is' k l@ means that any @'Optics.Optic.Optic' k@ can be used
-- as an @'Optics.Optic.Optic' l@. For example, we have an @'Is' 'A_Lens'
-- 'A_Traversal'@ instance, but not @'Is' 'A_Traversal' 'A_Lens'@.
--
-- This class needs instances for all possible combinations of tags.
--
class IsFacts k l => Is k l where
  -- | Witness of the subtyping relationship.
  implies :: (Constraints k p => r) -> (Constraints l p => r)

-- | Additional facts about the 'Is' relation, used to drive type inference.
--
-- If you use 'Is' as a superclass, you will need to enable
-- @UndecidableSuperClasses@ on the module that defines the class.  This is
-- safe, because the only class that 'IsFacts' refers to is 'Is' itself, and it
-- does so without creating any cycles, but GHC cannot see this.
type IsFacts k l = ( IsFacts1 k l
                   , IsFacts2 k l
                   , Join k l ~ l  -- TODO: are there nice use cases for these equations?
                   , Join l k ~ l
--                   , IsPossible k l
                   )

-- If we don't use IsFacts, then @view mapped@ yields a single "A_Setter cannot
-- be used as A_Getter" error.  With the above definition (lacking Join
-- equations) we get errors for An_AffineFold and A_Fold as well as A_Getter.
-- If we include Join equations, we get yet more errors because the composition
-- is invalid.

-- Somehow we need to encode the fact that the constraints are valid only if the
-- Is constraint is actually solvable; in the overlappable catch-all case we
-- don't want to include IsFacts in the context (nor, ideally, would it be in
-- the reflexive case).  This seems problematic: when trying to solve a wanted
-- we need to inject some extra goals, except if the wanted can't be solved; but
-- what if the goal is only solvable because of the extra goals?

-- Removing the OVERLAPPABLE error instance yields "Could not deduce (Is
-- A_Setter A_Getter)" and nothing else, though. So that could be a way forward?
-- Replacing the OVERLAPPABLE version with monomorphic versions doesn't help,
-- because they still need to satsify the superclass, and hence end up yielding
-- all the errors.
--
-- The reflexive Is k k instance is also problematic; in code that requires Is
-- reflexivity for polymorphic k we end up needing IsFacts k k (or its
-- expansion).  Perhaps it is better to replace the polymorphic instance with
-- all the monomorphic instances?  But this means "These potential instances
-- exist" sections of type errors get rather longer.


-- This doesn't work, because GHC doesn't report unsolved derived errors if the
-- original wanted is unsolved; this happens only due to the overlappable
-- instance.
class IsPossible k l

instance (TypeError
  ('ShowType A_Setter ':<>: 'Text " cannot be used as " ':<>: 'ShowType A_Getter
   ':$$: 'Text "Perhaps you meant one of these:"
   ':$$: ShowEliminations (EliminationForms A_Getter)
  )) => IsPossible A_Setter A_Getter where

instance IsPossible k l

-- | If @'Is' k l@ holds, then transitivity implies that for all @m@ such that
-- @'Is' l m@, we have @'Is' k m@.  When @l@ is concrete, we can instantiate @m@
-- to be anything it covers.  This is enough to get transitivity (for concrete
-- @l@) because the superclass on 'Is' will recursively unfold 'IsFacts1'.
--
-- The practical upshot of this is that if, for example, you try to call a
-- function of type @Is k A_Fold => 'Optic' k s t a b -> ...@ when you have @Is
-- k A_Traversal@ in the context, it will just work, because @Is k A_Traversal@
-- has @Is k A_Fold@ as a superclass.
type family IsFacts1 (k :: OpticKind) (l :: OpticKind) :: Constraint where
  IsFacts1 k An_AffineFold      = Is k A_Fold
  IsFacts1 k An_AffineTraversal = (Is k A_Traversal, Is k An_AffineFold)
  IsFacts1 k A_Getter           = Is k An_AffineFold
  IsFacts1 k An_Iso             = k ~ An_Iso
  IsFacts1 k A_Lens             = (Is k An_AffineTraversal, Is k A_Getter)
  IsFacts1 k A_Traversal        = (Is k A_Fold, Is k A_Setter)
  IsFacts1 k l                  = ()

type family IsFacts2 (k :: OpticKind) (l :: OpticKind) :: Constraint where
  IsFacts2 An_AffineTraversal k = (Is A_Prism k, Is A_Lens k)
  IsFacts2 A_Lens k             = Is An_Iso k
  IsFacts2 A_Traversal k        = Is An_AffineTraversal k
  IsFacts2 k l                  = ()


-- | Every kind of optic can be used as itself.
--instance IsFacts k k => Is k k where
--  implies r = r

instance Is A_Fold A_Fold where
  implies r = r

instance Is A_Getter A_Getter where
  implies r = r

instance Is A_Review A_Review where
  implies r = r

instance Is A_Lens A_Lens where
  implies r = r

instance Is A_Prism A_Prism where
  implies r = r

instance Is A_Traversal A_Traversal where
  implies r = r

instance Is A_Setter A_Setter where
  implies r = r

instance Is An_Iso An_Iso where
  implies r = r

instance Is An_AffineTraversal An_AffineTraversal where
  implies r = r

instance Is An_AffineFold An_AffineFold where
  implies r = r

-- | Overlappable instance for a custom type error.
{-
instance {-# OVERLAPPABLE #-} (TypeError
  ('ShowType k ':<>: 'Text " cannot be used as " ':<>: 'ShowType l
   ':$$: 'Text "Perhaps you meant one of these:"
   ':$$: ShowEliminations (EliminationForms k)
  ), IsFacts k l) => Is k l where
  implies _ = error "unreachable"
-}

{-
instance (TypeError
  ('ShowType A_Setter ':<>: 'Text " cannot be used as " ':<>: 'ShowType A_Getter
   ':$$: 'Text "Perhaps you meant one of these:"
   ':$$: ShowEliminations (EliminationForms A_Setter)
  ), Is A_Setter An_AffineFold) => Is A_Setter A_Getter where
  implies _ = error "unreachable"

instance (TypeError
  ('ShowType A_Setter ':<>: 'Text " cannot be used as " ':<>: 'ShowType An_AffineFold
   ':$$: 'Text "Perhaps you meant one of these:"
   ':$$: ShowEliminations (EliminationForms A_Setter)
  ), Is A_Setter A_Fold) => Is A_Setter An_AffineFold where
  implies _ = error "unreachable"

instance (TypeError
  ('ShowType A_Setter ':<>: 'Text " cannot be used as " ':<>: 'ShowType A_Fold
   ':$$: 'Text "Perhaps you meant one of these:"
   ':$$: ShowEliminations (EliminationForms A_Setter)
  )) => Is A_Setter A_Fold where
  implies _ = error "unreachable"
-}


type family EliminationForms (k :: OpticKind) where
  EliminationForms An_AffineFold      = AffineFoldEliminations
  EliminationForms An_AffineTraversal = AffineTraversalEliminations
  EliminationForms A_Fold             = FoldEliminations
  EliminationForms A_Getter           = GetterEliminations
  EliminationForms An_Iso             = IsoEliminations
  EliminationForms A_Lens             = LensEliminations
  EliminationForms A_Prism            = PrismEliminations
  EliminationForms A_ReversedLens     = ReviewEliminations
  EliminationForms A_ReversedPrism    = GetterEliminations
  EliminationForms A_Review           = ReviewEliminations
  EliminationForms A_Setter           = SetterEliminations
  EliminationForms A_Traversal        = TraversalEliminations

type AffineFoldEliminations = '( '[ '("preview", "Optics.AffineFold") ]
                               , '[ "(^?)" ])

type AffineTraversalEliminations = AffineFoldEliminations
              `AppendEliminations` SetterEliminations

type FoldEliminations = '( '[ '("traverseOf_", "Optics.Fold")
                            , '("foldMapOf",   "Optics.Fold")
                            , '("toListOf",    "Optics.Fold")
                            ]
                         , '[ "(^..)" ])

type GetterEliminations = '( '[ '("view", "Optics.Getter") ]
                           , '[ "(^.)" ])

type IsoEliminations = GetterEliminations
  `AppendEliminations` ReviewEliminations
  `AppendEliminations` SetterEliminations

type LensEliminations = GetterEliminations
   `AppendEliminations` SetterEliminations

type PrismEliminations = AffineFoldEliminations
    `AppendEliminations` ReviewEliminations
    `AppendEliminations` SetterEliminations

type ReviewEliminations = '( '[ '("review", "Optics.Review") ]
                           , '[ "(#)" ])

type SetterEliminations = '( '[ '("over", "Optics.Setter")
                              , '("set",  "Optics.Setter")
                              ]
                           , '[ "(%~)", "(.~)" ])

type TraversalEliminations = '( '[ '("traverseOf", "Optics.Traversal") ]
                              , '[]) `AppendEliminations` FoldEliminations
                                     `AppendEliminations` SetterEliminations

----------------------------------------

-- BEGIN GENERATED CONTENT

-- An_Iso
instance Is An_Iso             A_ReversedLens     where implies r = r
instance Is An_Iso             A_ReversedPrism    where implies r = r
instance Is An_Iso             A_Prism            where implies r = r
instance Is An_Iso             A_Review           where implies r = r
instance Is An_Iso             A_Lens             where implies r = r
instance Is An_Iso             A_Getter           where implies r = r
instance Is An_Iso             An_AffineTraversal where implies r = r
instance Is An_Iso             An_AffineFold      where implies r = r
instance Is An_Iso             A_Traversal        where implies r = r
instance Is An_Iso             A_Fold             where implies r = r
instance Is An_Iso             A_Setter           where implies r = r
-- A_ReversedLens
instance Is A_ReversedLens     A_Review           where implies r = r
-- A_ReversedPrism
instance Is A_ReversedPrism    A_Getter           where implies r = r
instance Is A_ReversedPrism    An_AffineFold      where implies r = r
instance Is A_ReversedPrism    A_Fold             where implies r = r
-- A_Prism
instance Is A_Prism            A_Review           where implies r = r
instance Is A_Prism            An_AffineTraversal where implies r = r
instance Is A_Prism            An_AffineFold      where implies r = r
instance Is A_Prism            A_Traversal        where implies r = r
instance Is A_Prism            A_Fold             where implies r = r
instance Is A_Prism            A_Setter           where implies r = r
-- A_Lens
instance Is A_Lens             A_Getter           where implies r = r
instance Is A_Lens             An_AffineTraversal where implies r = r
instance Is A_Lens             An_AffineFold      where implies r = r
instance Is A_Lens             A_Traversal        where implies r = r
instance Is A_Lens             A_Fold             where implies r = r
instance Is A_Lens             A_Setter           where implies r = r
-- A_Getter
instance Is A_Getter           An_AffineFold      where implies r = r
instance Is A_Getter           A_Fold             where implies r = r
-- An_AffineTraversal
instance Is An_AffineTraversal An_AffineFold      where implies r = r
instance Is An_AffineTraversal A_Traversal        where implies r = r
instance Is An_AffineTraversal A_Fold             where implies r = r
instance Is An_AffineTraversal A_Setter           where implies r = r
-- An_AffineFold
instance Is An_AffineFold      A_Fold             where implies r = r
-- A_Traversal
instance Is A_Traversal        A_Fold             where implies r = r
instance Is A_Traversal        A_Setter           where implies r = r

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
  Join A_Getter           An_AffineTraversal = An_AffineFold
  Join A_Getter           An_AffineFold      = An_AffineFold
  Join A_Getter           A_Traversal        = A_Fold
  Join A_Getter           A_Fold             = A_Fold
  -- no Join with         A_Setter

  -- An_AffineTraversal-----
  Join An_AffineTraversal An_Iso             = An_AffineTraversal
  -- no Join with         A_ReversedLens
  Join An_AffineTraversal A_ReversedPrism    = An_AffineFold
  Join An_AffineTraversal A_Prism            = An_AffineTraversal
  -- no Join with         A_Review
  Join An_AffineTraversal A_Lens             = An_AffineTraversal
  Join An_AffineTraversal A_Getter           = An_AffineFold
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
