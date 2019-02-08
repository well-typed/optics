-- |
--
-- Module: Optics
-- Description: The main module, usually you only need to import this one.
--
-- Introduction...
--
-- TODO: motivation behind @optics@
--
module Optics
  (
  -- * Basic usage
  -- $basicusage

  -- TODO: explain indexed optics

  -- * Differences from @lens@
  -- $differences

  -- * Core definitions

  -- | "Optics.Optic" module provides core definitions:
  --
  -- * Opaque 'Optic' type,
  --
  -- * which is parameterised over a type representing an optic flavour;
  --
  -- * 'Is' and 'Join' relations, illustrated in the graph below;
  --
  -- * and optic composition operators '%' and '%%'.
  --
  -- <<optics.png Optics hierarchy>>
  --
  -- The arrows represent 'Is' relation (partial order). The hierachy is a 'Join' semilattice, for example the
  -- 'Join' of a 'Lens' and a 'Prism' is an 'AffineTraversal'.
  --
  -- >>> :kind! Join A_Lens A_Prism
  -- Join A_Lens A_Prism :: *
  -- = An_AffineTraversal
  --
  -- There are also indexed variants of 'Traversal', 'Fold' and 'Setter'.
  -- Indexed optics are explained in more detail in /Differences from lens/ section.
  --
    module Optics.Optic

  -- * Optic variants

  -- |
  --
  -- There are 16 (/TODO/: add modules for LensyReview and PrismaticGetter)
  -- different kinds of optics, each documented in a separate module.
  -- Each optic module documentation has /formation/, /introduction/,
  -- /elimination/, and /well-formedness/ sections.
  --
  -- * The __formation__ sections contain type definitions. For example
  --
  --     @
  --     -- Tag for a lens.
  --     data A_Lens
  --
  --     -- Type synonym for a type-modifying lens.
  --     type 'Lens' s t a b = 'Optic' 'A_Lens' NoIx s t a b
  --     @
  --
  -- * In the __introduction__ sections are described the ways to construct
  --   the particular optic. Continuing with a 'Lens' example:
  --
  --     @
  --     -- Build a lens from a getter and a setter.
  --     'lens' :: (s -> a) -> (s -> b -> t) :: 'Lens' s t a b
  --     @
  --
  -- * In the __elimination__ sections are shown how you can destruct the
  --   optic into a pieces it was constructed from.
  --
  --     @
  --     -- 'Lens' is a 'Setter' and a 'Getter', therefore you can
  --
  --     'view' :: 'Lens' s t a b -> s -> a
  --     'set'  :: 'Lens' s t a b -> b -> s -> t
  --     'over' :: 'Lens' s t a b -> (a -> b) -> s -> t
  --     @
  --
  -- * __Computation__ rules tie introduction and
  --   elimination combinators together. These rules are automatically
  --   fulfilled.
  --
  --     @
  --     'view' ('lens' f g)   s = f s
  --     'set'  ('lens' f g) a s = g s a
  --     @
  --
  -- * All optics provided by the library are __well-formed__.
  --     Constructing of ill-formed optics is possible, but should be avoided.
  --     Ill-formed optic /might/ behave differently from what computation rules specify.
  --
  --     A 'Lens' should obey three laws, known as /GetPut/, /PutGet/ and /PutPut/.
  --     See "Optics.Lens" module for their definitions.
  --
  -- /Note:/ you should also consult the optics hierarchy diagram.
  -- Neither introduction or elimination sections list all ways to construct or use
  -- particular optic kind.
  -- For example you can construct 'Lens' from 'Iso' using 'castOptic'.
  -- Also, as a 'Lens' is also a 'Traversal', a 'Fold' etc, so you can use 'traverseOf', 'preview'
  -- and many other combinators.
  --
  , module O

  -- * Optics utilities

  -- ** Re

  -- | Some optics can be reversed with 're':
  -- @'Iso' s t a b@ into @'Iso' b a t s@,
  -- @'Getter' s t a b@ into @'Review' b a t s@ etc.
  -- Red arrows illustrate how 're' transforms optics:
  --
  -- <<reoptics.png Reversed Optics>>
  --
  -- 're' is mainly useful to invert 'Iso's:
  --
  -- >>> let _Identity = iso runIdentity Identity
  -- >>> view (_1 % re _Identity) ('x', "yz")
  -- Identity 'x'
  --
  -- Yet we can use a 'Lens' as a 'Review' too:
  --
  -- >>> review (re _1) ('x', "yz")
  -- 'x'
  --
  -- /Note:/ there are no @from@ combinator.

  , module Optics.Re

  -- ** Indexed optics

  -- |
  --
  -- @optics@ library also provides indexed optics, which provide
  -- an additional /index/ value in mappings:
  --
  -- @
  -- 'over'  :: 'Setter'     s t a b -> (a -> b)      -> s -> t
  -- 'iover' :: 'IxSetter' i s t a b -> (i -> a -> b) -> s -> t
  -- @
  --
  -- Note that there aren't any laws about indices.
  -- Especially in compositions same index may occur multiple times.
  --
  -- The machinery builds on indexed variants of 'Functor', 'Foldable', and 'Traversable' classes:
  -- 'FunctorWithIndex', 'FoldableWithIndex' and 'TraversableWithIndex' respectively.
  -- There are instances for types in the boot libraries.
  --
  -- @
  -- class ('FoldableWithIndex' i t, 'Traversable' t)
  --   => 'TraversableWithIndex' i t | t -> i where
  --     'itraverse' :: 'Applicative' f => (i -> a -> f b) -> t a -> f (t b)
  -- @
  --
  -- Indexed optics /can/ be used as regular ones, i.e. indexed optics
  -- gracefully downgrade to regular ones.
  --
  -- >>> toListOf ifolded "foo"
  -- "foo"
  --
  -- But there is also a combinator to explicitly erase indices:
  --
  -- >>> :t (ifolded % simple)
  -- (ifolded % simple)
  --   :: FoldableWithIndex i f => Optic A_Fold '[i] (f b) (f b) b b
  --
  -- >>> :t noIx (ifolded % simple)
  -- noIx (ifolded % simple)
  --   :: FoldableWithIndex i f => Optic A_Fold NoIx (f b) (f b) b b
  --
  -- 'noIx' can erase all indices
  --
  -- >>> :t noIx (ifolded % ifolded)
  -- noIx (ifolded % ifolded)
  --   :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
  --      Optic A_Fold NoIx (f1 (f2 b)) (f1 (f2 b)) b b
  --
  -- As the example above illustrates (/TODO:/ will do),
  -- regular and indexed optics have the same kind, in this case @'Optic' 'A_Fold'@.
  -- Regular optics simply don't have any indices.
  -- The provided type aliases `IxFold`, `IxSetter` and `IxTraversal`
  -- are variants with a single index.
  --
  -- In the diagram below, the optics hierachy is amended with these (singly) indexed variants (in blue).
  -- Orange arrows mean
  -- "can be used as one, assuming it's composed with any optic below the
  -- orange arrow first". For example. '_1' is not an indexed fold, but
  -- @'itraversed' % '_1'@ is, because it's an indexed traversal, so it's
  -- also an indexed fold.
  --
  -- >>> let fst' = _1 :: Lens (a, c) (b, c) a b
  -- >>> :t fst' % itraversed
  -- fst' % itraversed
  --   :: TraversableWithIndex i t =>
  --      Optic A_Traversal (WithIx i) (t a, c) (t b, c) a b
  --
  -- <<indexedoptics.png Indexed Optics>>
  --
  -- /TODO:/ write about 'icompose' and multiple indices.
  --
  , module Optics.Indexed

  -- * At
  , module Optics.At

  -- * Cons
  , module Optics.Cons

  -- ** Each

  -- | A 'Traversal' for a (potentially monomorphic) container.
  --
  -- >>> over each (*10) (1,2,3)
  -- (10,20,30)
  --
  , module Optics.Each

  -- * Empty
  , module Optics.Empty

  -- * View
  , module Optics.View

  -- * Zoom
  , module Optics.Zoom

  -- * Generation of optics with Template Haskell
  , module Optics.TH

  -- * Optics for concrete base types
  , module P
  )
  where

-- Core optics functionality

-- for some reason haddock reverses the list...

import Optics.Optic

import Optics.Traversal                      as O
import Optics.AffineTraversal                as O
import Optics.Setter                         as O
import Optics.Review                         as O
import Optics.PrismaticGetter                as O
import Optics.Prism                          as O
import Optics.LensyReview                    as O
import Optics.Lens                           as O
import Optics.IxSetter                       as O
import Optics.IxTraversal                    as O
import Optics.IxAffineTraversal              as O
import Optics.IxFold                         as O
import Optics.IxAffineFold                   as O
import Optics.Iso                            as O
import Optics.Getter                         as O
import Optics.Fold                           as O
import Optics.AffineFold                     as O
import Optics.Equality                       as O

-- Optics utilities
import Optics.At
import Optics.Cons
import Optics.Each
import Optics.Empty
import Optics.Indexed
import Optics.Re
import Optics.View
import Optics.Zoom

-- Template Haskell support
import Optics.TH

-- Optics for concrete base types

import Data.Tuple.Optics                     as P
import Data.Maybe.Optics                     as P
import Data.Either.Optics                    as P

-- $basicusage
--
-- @
-- import "Optics"
-- @
--
-- and then...
--
-- Operators (if you prefer them) are in
--
-- @
-- import "Optics.Operators"
-- @
--


-- $differences
--
-- /This section is work-in-progress/
--
-- === From Adam's talk:
--
-- See @Talk.pdf@, or watch <https://skillsmatter.com/skillscasts/10692-through-a-glass-abstractly-lenses-and-the-power-of-abstraction>
--
-- * @optics@ has an abstract interface: 'Optic' is an opaque type
-- * Cannot write @optics@ without depending on the package,
--   therefore @optics-core@ doesnt' have non GHC-boot library dependencies.
--   (one cannot write /prisms/ with @lens@ without depending on @profunctors@, indexed optics require depending on @lens@ ...)
-- * abstract interface: @optics@ has better error messages (note: @silica@ is a hybrid approach)
--
--     >>> set (to fst)
--     ...
--     ...A_Getter cannot be used as A_Setter
--     ...
--
-- * abstract interface: better type-inference (optics kind is preserved)
--
--     >>> :t traversed % to not
--     traversed % to not
--       :: Traversable t => Optic A_Fold NoIx (t Bool) (t Bool) Bool Bool
--
-- * abstract interface: not all optics have 'Join'
--
--     >>> sets map % to not
--     ...
--     ...A_Setter cannot be composed with A_Getter
--     ...
--
-- * 'Optic' is a rank 1 type, so there is no need for @ALens@ etc.
-- * Types that say what they mean
-- * More comprehensible type errors
-- * Less vulnerable to the monomorphism restriction
-- * Free choice of lens implementation
-- * Indexed optics have different interface.
--
-- === Drawbacks
--
-- * Can’t insert points into the subtyping order post hoc
--
-- === Technical differences
--
-- * Composition operator is '%'
-- * 'set'' is a strict version of 'set', not 'set' for type-preserving optics
-- * 'view' is for getters
-- * 'preview' is for affine folds
-- * 'foldOf' is the equivalent of view for folds
-- * 'firstOf' is now 'headOf'
-- * Position lenses up to '_9' instead of _19 are provided
-- * 'Each' provides indexed traversals
-- * There are four variants of 'backwards' for (indexed) folds and traversals
-- * None of operators is exported from main module
-- * All ordinary optics are index-preserving by default
-- * Indexed optics interface is different (let's expand in own section, when the implementation is stabilised)
-- * There is no @IndexedLens@, @IndexedGetter@, @Traversal1@ nor @Fold1@.
-- * There is 'AffineTraversal', 'AffineFold', 'IxAffineTraversal' and 'IxAffineFold'
-- * We can't use 'traverse' as an optic directly, but there is a 'Traversal' called 'traversed'.
-- * 'gview' is compatible with @lens@, but it uses a type class which chooses between
--   'view', 'preview' and 'foldOf' (See discussion in <https://github.com/well-typed/optics/issues/57 GitHub #57>: Do we need 'gview' at all, and what 'Optics.Operators.^.' should be)

-- * There are no 'from', only 're' (Should there be a 'from' restricted to 'Iso' or an alias to 're'? <https://github.com/well-typed/optics/pull/43#discussion_r247121380>)
--

-- $setup
-- >>> import Data.Functor.Identity
