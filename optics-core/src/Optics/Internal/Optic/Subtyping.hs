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
module Optics.Internal.Optic.Subtyping
  ( Is(..)
  , JoinKinds(..)
  ) where

import Data.Kind (Constraint)
import Data.Type.Bool
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
class Is k l where
  -- | Witness of the subtyping relationship.
  implies :: (Constraints k p => r) -> (Constraints l p => r)

-- | Every kind of optic can be used as itself.
instance Is k k where
  implies r = r

-- | Overlappable instance for a custom type error.
instance {-# OVERLAPPABLE #-} Isn't k l => Is k l where
  implies _ = error "unreachable"

-- | Hide the error message in a type family, for prettier Haddocks.  (We can't
-- just use a type synonym, or use a class with a single instance, because then
-- GHC reports the 'TypeError' at the definition site.)
type family Isn't k l where
  Isn't k l =
    TypeError ('ShowType k ':<>: 'Text " cannot be used as " ':<>: 'ShowType l
     ':$$: 'Text "Perhaps you meant one of these:"
     ':$$: ShowEliminations (EliminationForms k)
    )

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
-- In presence of a @JoinKinds k l m@ constraint @Optic m@ represents the least
-- upper bound of an @Optic k@ and an @Optic l@. This means in particular that
-- composition of an @Optic k@ and an @Optic k@ will yield an @Optic m@.
--
-- @since 0.4
--
class JoinKinds k l m | k l -> m where
  joinKinds :: ((Constraints k p, Constraints l p) => r) -> (Constraints m p => r)

-- BEGIN GENERATED CONTENT

-- An_Iso -----
instance k ~ An_Iso             => JoinKinds An_Iso             An_Iso             k where
  joinKinds r = r
instance k ~ A_ReversedLens     => JoinKinds An_Iso             A_ReversedLens     k where
  joinKinds r = r
instance k ~ A_ReversedPrism    => JoinKinds An_Iso             A_ReversedPrism    k where
  joinKinds r = r
instance k ~ A_Prism            => JoinKinds An_Iso             A_Prism            k where
  joinKinds r = r
instance k ~ A_Review           => JoinKinds An_Iso             A_Review           k where
  joinKinds r = r
instance k ~ A_Lens             => JoinKinds An_Iso             A_Lens             k where
  joinKinds r = r
instance k ~ A_Getter           => JoinKinds An_Iso             A_Getter           k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds An_Iso             An_AffineTraversal k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_Iso             An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds An_Iso             A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds An_Iso             A_Fold             k where
  joinKinds r = r
instance k ~ A_Setter           => JoinKinds An_Iso             A_Setter           k where
  joinKinds r = r

-- A_ReversedLens -----
instance k ~ A_ReversedLens     => JoinKinds A_ReversedLens     A_ReversedLens     k where
  joinKinds r = r
instance k ~ A_ReversedLens     => JoinKinds A_ReversedLens     An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_ReversedLens     A_ReversedPrism
instance k ~ A_Review           => JoinKinds A_ReversedLens     A_Prism            k where
  joinKinds r = r
instance k ~ A_Review           => JoinKinds A_ReversedLens     A_Review           k where
  joinKinds r = r
--                              no JoinKinds A_ReversedLens     A_Lens
--                              no JoinKinds A_ReversedLens     A_Getter
--                              no JoinKinds A_ReversedLens     An_AffineTraversal
--                              no JoinKinds A_ReversedLens     An_AffineFold
--                              no JoinKinds A_ReversedLens     A_Traversal
--                              no JoinKinds A_ReversedLens     A_Fold
--                              no JoinKinds A_ReversedLens     A_Setter

-- A_ReversedPrism -----
instance k ~ A_ReversedPrism    => JoinKinds A_ReversedPrism    A_ReversedPrism    k where
  joinKinds r = r
instance k ~ A_ReversedPrism    => JoinKinds A_ReversedPrism    An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_ReversedPrism    A_ReversedLens
instance k ~ An_AffineFold      => JoinKinds A_ReversedPrism    A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_ReversedPrism    A_Review
instance k ~ A_Getter           => JoinKinds A_ReversedPrism    A_Lens             k where
  joinKinds r = r
instance k ~ A_Getter           => JoinKinds A_ReversedPrism    A_Getter           k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_ReversedPrism    An_AffineTraversal k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_ReversedPrism    An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_ReversedPrism    A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_ReversedPrism    A_Fold             k where
  joinKinds r = r
--                              no JoinKinds A_ReversedPrism    A_Setter

-- A_Prism -----
instance k ~ A_Prism            => JoinKinds A_Prism            A_Prism            k where
  joinKinds r = r
instance k ~ A_Prism            => JoinKinds A_Prism            An_Iso             k where
  joinKinds r = r
instance k ~ A_Review           => JoinKinds A_Prism            A_ReversedLens     k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Prism            A_ReversedPrism    k where
  joinKinds r = r
instance k ~ A_Review           => JoinKinds A_Prism            A_Review           k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds A_Prism            A_Lens             k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Prism            A_Getter           k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds A_Prism            An_AffineTraversal k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Prism            An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds A_Prism            A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Prism            A_Fold             k where
  joinKinds r = r
instance k ~ A_Setter           => JoinKinds A_Prism            A_Setter           k where
  joinKinds r = r

-- A_Review -----
instance k ~ A_Review           => JoinKinds A_Review           A_Review           k where
  joinKinds r = r
instance k ~ A_Review           => JoinKinds A_Review           An_Iso             k where
  joinKinds r = r
instance k ~ A_Review           => JoinKinds A_Review           A_ReversedLens     k where
  joinKinds r = r
--                              no JoinKinds A_Review           A_ReversedPrism
instance k ~ A_Review           => JoinKinds A_Review           A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_Review           A_Lens
--                              no JoinKinds A_Review           A_Getter
--                              no JoinKinds A_Review           An_AffineTraversal
--                              no JoinKinds A_Review           An_AffineFold
--                              no JoinKinds A_Review           A_Traversal
--                              no JoinKinds A_Review           A_Fold
--                              no JoinKinds A_Review           A_Setter

-- A_Lens -----
instance k ~ A_Lens             => JoinKinds A_Lens             A_Lens             k where
  joinKinds r = r
instance k ~ A_Lens             => JoinKinds A_Lens             An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_Lens             A_ReversedLens
instance k ~ A_Getter           => JoinKinds A_Lens             A_ReversedPrism    k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds A_Lens             A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_Lens             A_Review
instance k ~ A_Getter           => JoinKinds A_Lens             A_Getter           k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds A_Lens             An_AffineTraversal k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Lens             An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds A_Lens             A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Lens             A_Fold             k where
  joinKinds r = r
instance k ~ A_Setter           => JoinKinds A_Lens             A_Setter           k where
  joinKinds r = r

-- A_Getter -----
instance k ~ A_Getter           => JoinKinds A_Getter           A_Getter           k where
  joinKinds r = r
instance k ~ A_Getter           => JoinKinds A_Getter           An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_Getter           A_ReversedLens
instance k ~ A_Getter           => JoinKinds A_Getter           A_ReversedPrism    k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Getter           A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_Getter           A_Review
instance k ~ A_Getter           => JoinKinds A_Getter           A_Lens             k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Getter           An_AffineTraversal k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds A_Getter           An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Getter           A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Getter           A_Fold             k where
  joinKinds r = r
--                              no JoinKinds A_Getter           A_Setter

-- An_AffineTraversal -----
instance k ~ An_AffineTraversal => JoinKinds An_AffineTraversal An_AffineTraversal k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds An_AffineTraversal An_Iso             k where
  joinKinds r = r
--                              no JoinKinds An_AffineTraversal A_ReversedLens
instance k ~ An_AffineFold      => JoinKinds An_AffineTraversal A_ReversedPrism    k where
  joinKinds r = r
instance k ~ An_AffineTraversal => JoinKinds An_AffineTraversal A_Prism            k where
  joinKinds r = r
--                              no JoinKinds An_AffineTraversal A_Review
instance k ~ An_AffineTraversal => JoinKinds An_AffineTraversal A_Lens             k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_AffineTraversal A_Getter           k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_AffineTraversal An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds An_AffineTraversal A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds An_AffineTraversal A_Fold             k where
  joinKinds r = r
instance k ~ A_Setter           => JoinKinds An_AffineTraversal A_Setter           k where
  joinKinds r = r

-- An_AffineFold -----
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      An_AffineFold      k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      An_Iso             k where
  joinKinds r = r
--                              no JoinKinds An_AffineFold      A_ReversedLens
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      A_ReversedPrism    k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      A_Prism            k where
  joinKinds r = r
--                              no JoinKinds An_AffineFold      A_Review
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      A_Lens             k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      A_Getter           k where
  joinKinds r = r
instance k ~ An_AffineFold      => JoinKinds An_AffineFold      An_AffineTraversal k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds An_AffineFold      A_Traversal        k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds An_AffineFold      A_Fold             k where
  joinKinds r = r
--                              no JoinKinds An_AffineFold      A_Setter

-- A_Traversal -----
instance k ~ A_Traversal        => JoinKinds A_Traversal        A_Traversal        k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds A_Traversal        An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_Traversal        A_ReversedLens
instance k ~ A_Fold             => JoinKinds A_Traversal        A_ReversedPrism    k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds A_Traversal        A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_Traversal        A_Review
instance k ~ A_Traversal        => JoinKinds A_Traversal        A_Lens             k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Traversal        A_Getter           k where
  joinKinds r = r
instance k ~ A_Traversal        => JoinKinds A_Traversal        An_AffineTraversal k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Traversal        An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Traversal        A_Fold             k where
  joinKinds r = r
instance k ~ A_Setter           => JoinKinds A_Traversal        A_Setter           k where
  joinKinds r = r

-- A_Fold -----
instance k ~ A_Fold             => JoinKinds A_Fold             A_Fold             k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Fold             An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_Fold             A_ReversedLens
instance k ~ A_Fold             => JoinKinds A_Fold             A_ReversedPrism    k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Fold             A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_Fold             A_Review
instance k ~ A_Fold             => JoinKinds A_Fold             A_Lens             k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Fold             A_Getter           k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Fold             An_AffineTraversal k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Fold             An_AffineFold      k where
  joinKinds r = r
instance k ~ A_Fold             => JoinKinds A_Fold             A_Traversal        k where
  joinKinds r = r
--                              no JoinKinds A_Fold             A_Setter

-- A_Setter -----
instance k ~ A_Setter           => JoinKinds A_Setter           A_Setter           k where
  joinKinds r = r
instance k ~ A_Setter           => JoinKinds A_Setter           An_Iso             k where
  joinKinds r = r
--                              no JoinKinds A_Setter           A_ReversedLens
--                              no JoinKinds A_Setter           A_ReversedPrism
instance k ~ A_Setter           => JoinKinds A_Setter           A_Prism            k where
  joinKinds r = r
--                              no JoinKinds A_Setter           A_Review
instance k ~ A_Setter           => JoinKinds A_Setter           A_Lens             k where
  joinKinds r = r
--                              no JoinKinds A_Setter           A_Getter
instance k ~ A_Setter           => JoinKinds A_Setter           An_AffineTraversal k where
  joinKinds r = r
--                              no JoinKinds A_Setter           An_AffineFold
instance k ~ A_Setter           => JoinKinds A_Setter           A_Traversal        k where
  joinKinds r = r
--                              no JoinKinds A_Setter           A_Fold

-- END GENERATED CONTENT

-- | Overlappable instance for a custom type error.
instance {-# OVERLAPPABLE #-}
  ( JoinKinds k l m
  , CannotJoinKinds k l m p v
  ) => JoinKinds k l m where
  joinKinds _ = error "unreachable"


-- | This type family is called when solving @JoinKinds k l m@ fails, with @p@
-- and @v@ being fresh variables.  It returns a constraint which, when solved,
-- displays a nice type error.
--
-- We know that @k@ and @l@ must be concrete, because otherwise GHC would not
-- have committed to the OVERLAPPABLE instance above that uses CannotJoinKinds.
-- However @m@ may or may not be concrete, and we want to print a different
-- error in each case (#423).
--
-- This could be a type synonym, except then GHC prints the type error at the
-- definition site of the type synonym, which is rather stupid.
--
type family CannotJoinKinds k l m actual is_concrete :: Constraint where
  CannotJoinKinds k l m actual is_concrete =
    ( -- 1. Unify is_concrete with True or False
      IsConcrete m is_concrete
      -- 2. If is_concrete, unify actual with the join of k and m (otherwise
      -- do nothing; the conditional is necessary to avoid an infinite
      -- constraint solver loop).
    , If is_concrete (JoinKinds k l actual) (() :: Constraint)
      -- 3. Display an error that depends on is_concrete and actual
    , TypeError (CannotJoinKindsMessage k l m actual is_concrete)
    )

-- | If @is_concrete@ is True, then the context must be demanding an optic kind
-- that differs from the actual result of the composition (with the latter given
-- by the @actual@ parameter).  For example:
--
-- >>> :t castOptic @A_Setter @A_Setter (traversed % _1)
-- ...
-- ... • A_Traversal composed with A_Lens produces A_Traversal,
-- ...   but the context expects A_Setter
-- ...
--
-- Otherwise, the user has tried to compose two optic kinds that simply cannot
-- be used together, for example:
--
-- >>> to not % mapped
-- ...
-- ... • A_Getter cannot be composed with A_Setter
-- ...
--
type family CannotJoinKindsMessage k l m actual is_concrete where
  CannotJoinKindsMessage k l m actual 'True
    = 'ShowType k ':<>: 'Text " composed with " ':<>: 'ShowType l
      ':<>: 'Text " produces " ':<>: 'ShowType actual ':<>: 'Text ","
      ':$$: 'Text "but the context expects " ':<>: 'ShowType m
  CannotJoinKindsMessage k l m actual 'False
    = 'ShowType k ':<>: 'Text " cannot be composed with " ':<>: 'ShowType l

-- | When solving an @IsConcrete m v@ constraint, if @m@ is concrete (i.e. a
-- type constructor) then @is_concrete@ will be unified with True; otherwise
-- (i.e. @m@ is a type variable or family application), @is_concrete@ will be
-- unified with @False@.
--
-- Note that applications are considered as concrete, even if the head is a
-- variable.  This means we get the following (but no user is likely to write
-- such nonsense):
--
-- >>> castOptic @(_ _) (traversed % _1)
-- ...
-- ... • A_Traversal cannot be used as ...
-- ...
--
type IsConcrete m is_concrete = IsConcreteHelper (PartialIsConcrete m) is_concrete

-- | If @m@ is concrete, it is apart from 'Void0' and hence this will reduce to
-- True.  Otherwise the family application gets stuck.
type family PartialIsConcrete m where
  PartialIsConcrete Void0 = 'False
  PartialIsConcrete _     = 'True

-- | If the first parameter is True, unify the second parameter with True.  If
-- the first parameter is anything else (crucially, including a stuck type
-- family application) unify the second parameter with False.
class IsConcreteHelper x is_concrete
instance                    is_concrete ~ 'True  => IsConcreteHelper 'True is_concrete
instance {-# INCOHERENT #-} is_concrete ~ 'False => IsConcreteHelper f     is_concrete

-- | Non-exported, used solely to be apart from every other concrete type.
data Void0

-- $setup
-- >>> import Optics.Core
