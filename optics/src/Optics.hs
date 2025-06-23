-- |
--
-- Module: Optics
-- Description: The main module, usually you only need to import this one.
--
-- This library makes it possible to define and use 'Lens'es, 'Traversal's,
-- 'Prism's and other /optics/, using an /abstract interface/.
--
module Optics
  (
  -- * Introduction
  -- $introduction

  -- ** What are optics?
  -- $what

  -- ** What is the abstract interface?
  -- $abstract

  -- ** Comparison with @lens@
  -- $lens_comparison

  -- ** Other resources
  -- $otherresources

  -- * Using the library
  -- $basicusage
    module Optics.Optic

  -- ** Optic kinds #optickinds#
  , module O

  -- ** Optic operators
  , module Optics.Operators

  -- * Optics utilities

  -- ** At

  -- | An 'AffineTraversal' to traverse a key in a map or an element of a
  -- sequence:
  --
  -- >>> preview (ix 1) ['a','b','c']
  -- Just 'b'
  --
  -- a 'Lens' to get, set or delete a key in a map:
  --
  -- >>> set (at 0) (Just 'b') (Map.fromList [(0, 'a')])
  -- fromList [(0,'b')]
  --
  -- and a 'Lens' to insert or remove an element of a set:
  --
  -- >>> IntSet.fromList [1,2,3,4] & contains 3 .~ False
  -- fromList [1,2,4]
  --
  , module Optics.At

  -- ** Cons

  -- | 'Prism's to match on the left or right side of a list, vector or other
  -- sequential structure:
  --
  -- >>> preview _Cons "abc"
  -- Just ('a',"bc")
  --
  -- >>> preview _Snoc "abc"
  -- Just ("ab",'c')
  --
  , module Optics.Cons

  -- ** Each

  -- | An 'IxTraversal' for each element of a (potentially monomorphic) container.
  --
  -- >>> over each (*10) (1,2,3)
  -- (10,20,30)
  --
  , module Optics.Each

  -- ** Empty

  -- | A 'Prism' for a container type that may be empty.
  --
  -- >>> isn't _Empty [1,2,3]
  -- True
  --
  , module Optics.Empty

  -- ** Generic data access
  , module Optics.Generic

  -- ** Re

  -- | Some optics can be reversed with 're'.  This is mainly useful to invert
  -- 'Iso's:
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
  -- In the following diagram, red arrows illustrate how 're' transforms optics.
  -- The 'ReversedLens' and 'ReversedPrism' optic kinds are backwards versions
  -- of 'Lens' and 'Prism' respectively, and are present so that @'re' . 're'@
  -- does not change the optic kind.
  --
  -- <<diagrams/reoptics.png Reversed Optics>>
  --
  , module Optics.Re

  -- ** ReadOnly

  -- | Defines 'getting', which turns a read-write optic into its read-only
  -- counterpart.
  , module Optics.ReadOnly

  -- ** Mapping

  -- | Defines 'mapping' through 'Functor's
  , module Optics.Mapping

  -- ** 'Setter' utilities for working in 'Control.Monad.State.MonadState'.
  , module Optics.State

  -- ** View

  -- | A generalized view function 'gview', which returns a single result (like
  -- 'view') if the optic is a 'Getter', a 'Maybe' result (like 'preview') if
  -- the optic is an 'AffineFold', or a monoidal summary of results (like
  -- 'foldOf') if the optic is a 'Fold'.  In addition, it works for any
  -- 'Control.Monad.Reader.MonadReader', not just @(->)@.
  --
  -- >>> gview _1 ('x','y')
  -- 'x'
  --
  -- >>> gview _Left (Left 'x')
  -- Just 'x'
  --
  -- >>> gview folded ["a", "b"]
  -- "ab"
  --
  -- >>> runReaderT (gview _1) ('x','y') :: IO Char
  -- 'x'
  --
  -- This module is experimental.  Using the more type-restricted variants is
  -- encouraged where possible.
  --
  , module Optics.View

  -- ** Zoom

  -- | A class to 'zoom' in, changing the 'Control.Monad.State.State' supplied
  -- by many different monad transformers, potentially quite deep in a monad
  -- transformer stack.
  --
  -- >>> flip execState ('a','b') $ zoom _1 $ equality .= 'c'
  -- ('c','b')
  --
  , module Optics.Zoom

  -- * Indexed optics
  -- $indexed
  , module Optics.Indexed

  -- * Monoid structures #monoids#
  -- $monoids

  -- * Generation of optics
  -- ** ...with Template Haskell
  , module Optics.TH
  -- ** ...with @OverloadedLabels@
  , module Optics.Label

  -- * Optics for concrete base types
  , module P

  -- * Cheat sheet
  -- $cheatsheet
  )
  where

-- Core optics functionality

-- for some reason haddock reverses the list...

import Optics.Optic

import Optics.Traversal                      as O
import Optics.Setter                         as O
import Optics.Review                         as O
import Optics.ReversedPrism                  as O
import Optics.Prism                          as O
import Optics.ReversedLens                   as O
import Optics.Lens                           as O
import Optics.IxTraversal                    as O
import Optics.IxSetter                       as O
import Optics.IxFold                         as O
import Optics.IxAffineTraversal              as O
import Optics.IxAffineFold                   as O
import Optics.IxGetter                       as O
import Optics.IxLens                         as O
import Optics.Iso                            as O
import Optics.Getter                         as O
import Optics.Fold                           as O
import Optics.AffineTraversal                as O
import Optics.AffineFold                     as O

-- Optics utilities
import Optics.At
import Optics.Cons
import Optics.Each
import Optics.Empty
import Optics.Generic
import Optics.Indexed
import Optics.Mapping
import Optics.Operators
import Optics.Re
import Optics.ReadOnly
import Optics.State
import Optics.View
import Optics.Zoom

-- Overloaded labels support
import Optics.Label

-- Template Haskell support
import Optics.TH

-- Optics for concrete base types

import Data.Tuple.Optics                     as P
import Data.Maybe.Optics                     as P
import Data.Either.Optics                    as P

-- $introduction
--
-- Read on for a general introduction to the notion of optics, or if you are
-- familiar with them already, you may wish to jump ahead to the "What is the
-- abstract interface?"  section below in "Optics#abstract".

-- $what
--
-- An optic is a first-class, composable /notion of substructure/.  As a highly
-- abstract concept, the idea can be approached by considering several examples
-- of optics and understanding their common features. What are the possible
-- relationships between some "outer" type @S@ and some "inner" type @A@?
--
-- (For simplicity we will initially ignore the possibility of type-changing
-- update operations, which change @A@ to some other type @B@ and hence change
-- @S@ to some other type @T@.  These are fully supported by the library, at the
-- cost of some extra type parameters.)
--
-- === "Optics.Iso": isomorphisms
--
-- First, @S@ and @A@ may be /isomorphic/, i.e. there exist mutually inverse
-- functions to convert @S -> A@ and @A -> S@.  This is a somewhat trivial
-- notion of substructure: @A@ is just another way to represent "all of @S@".
--
-- An @'Iso'' S A@ is an isomorphism between @S@ and @A@, with the conversion
-- functions given by 'view' and 'review'. For example, given
--
-- @
-- newtype Age = Age Int
-- @
--
-- there is an isomorphism between the newtype and its representation:
--
-- @
--        'coerced' :: 'Iso'' Age 'Int'
-- 'view'   'coerced' :: Age -> 'Int'
-- 'review' 'coerced' :: 'Int' -> Age
-- @
--
-- === "Optics.Lens": generalised fields
--
-- If @S@ is a simple product type (i.e. it has a single constructor with one or
-- more fields), @A@ may be a single field of @S@.  More generally, @A@ may be
-- "part of @S@" in the sense that @S@ is isomorphic to the pair @(A,C)@ for
-- some type @C@ representing the other fields.  In this case, there is a
-- /projection/ function @S -> A@ for getting the value of the field, but the
-- /update/ function (setting the value of the field) requires the "rest of @S@"
-- and so has type @A -> S -> S@.
--
-- A @'Lens'' S A@ captures the structure of @A@ being a field of @S@, with the
-- projection function given by 'view' and the update function by 'set'.  For
-- example, for the pair type @(X,Y)@ there are lenses for each component:
--
-- @
--      '_1' :: 'Lens'' (X,Y) X
--      '_2' :: 'Lens'' (X,Y) Y
-- 'view' '_1' :: (X,Y) -> X
-- 'set'  '_2' :: Y -> (X,Y) -> (X,Y)
-- @
--
-- (Note that the update function could arguably have the more precise type @A
-- -> C -> S@, since we do not expect the result of setting a field to depend on
-- the previous value of the field.  However, making @C@ explicit turns out to
-- be awkward, so instead we impose /laws/ to require that the result of setting
-- the field depends only on @C@, and, more generally, that the lens behaves as
-- we would expect.)
--
-- === "Optics.Prism": generalised constructors
--
-- If @S@ is a simple sum type (i.e. it has one or more constructors, each with
-- a single field), @A@ may be the type of the field for a single constructor of
-- @S@.  More generally, @S@ may be isomorphic to the disjoint union @Either D
-- A@ for some type @D@ representing the other constructors.  In this case,
-- projecting out @A@ from @S@ (pattern-matching on the constructor) may fail,
-- so it has type @S -> Maybe A@.  In the reverse direction we have a function
-- of type @A -> S@ representing the constructor itself.
--
-- A @'Prism'' S A@ captures the structure of @A@ being a constructor of @S@,
-- with the partial projection function given by 'preview' and the constructor
-- function given by 'review'.  For example, for the type @'Either' X Y@ there
-- is a prism for each constructor:
--
-- @
--         '_Left'  :: 'Prism'' ('Either' X Y) X
--         '_Right' :: 'Prism'' ('Either' X Y) Y
-- 'preview' '_Left'  :: 'Either' X Y -> 'Maybe' X
-- 'review'  '_Right' :: Y -> 'Either' X Y
-- @
--
-- === "Optics.Traversal": multiple substructures
--
-- Alternatively, @S@ may "contain" the substructure @A@ a variable number of
-- times.  In this case, the projection function extracts the (possibly zero or
-- many) elements so has type @S -> [A]@, while the update function may take
-- different values for different elements so has type @(A -> A) -> S -> S@
-- (though in fact more general formulations are possible).
--
-- A @'Traversal'' S A@ captures the structure of @A@ being contained in @S@
-- perhaps multiple times, with the list of values given by 'toListOf' and the
-- update function given by 'over' .  For example, for the type @Maybe X@ there
-- is a traversal that may return zero or one element:
--
-- @
--          'traversed' :: 'Traversal'' ('Maybe' X) X
-- 'toListOf' 'traversed' :: 'Maybe' X -> [X]
-- 'over'     'traversed' :: (X -> X) -> 'Maybe' X -> 'Maybe' X
-- @
--
-- (In fact, traversals of at most one element are known as /affine/ traversals,
-- see "Optics.AffineTraversal".)
--
--
-- === In general
--
-- So far we have seen four different kinds of optic or "notions of
-- substructure", and many more are possible.  Observe the important properties
-- they have in common:
--
-- * There are subtyping relationships between different optic kinds.  Any
--   isomorphism is trivially a lens and a prism (with no other fields or
--   constructors, respectively).  Any lens is a traversal (where the list of
--   elements is always a singleton list), and any prism is also a traversal
--   (where there will be zero or one element depending on whether the
--   constructor matches).  This was implicit in the fact that we
--   used the same operators in multiple cases: 'view' gives the projection
--   function of both an isomorphism and a lens, but cannot be applied to a
--   traversal.
--
-- * Optics can be composed. If @S@ is isomorphic to @U@ and @U@ is isomorphic
--   to @A@ then @S@ is isomorphic to @A@, and similarly for other optic kinds.
--
-- * Composition and subtyping interact: a lens and a prism can be composed, by
--   first thinking of them as traversals using the subtyping relationship.  That
--   is, if @S@ has a field @U@, and @U@ has a constructor @A@, then @S@
--   contains zero or one @A@s that we can pick out with a traversal (but in
--   general there is neither a lens from @S@ to @A@ nor a prism).
--
-- * Each optic kind can be described by certain operations it enables. For
--   example lenses support projection and update, while prisms support partial
--   projection and construction.
--
-- * Optics are subject to laws, which are necessary for the operations to make
--   sense.
--
-- The point of the @optics@ library is to capture this common pattern.


-- $abstract #abstract#
--
-- A key principle behind this library is the belief that optics are useful as
-- an abstract concept, and that the purpose of types is to capture abstract
-- concepts and make them useful.  The programmer using optics should be able to
-- think in terms of the abstract interface, rather than the details of the
-- implementation, and implementation choices should (as far as possible) not
-- dictate the interface.
--
-- Each optic kind is identified by a "tag type" (such as 'A_Lens'), which is an
-- empty data type.  The type of the actual optics (such as 'Lens') is obtained
-- by applying the 'Optic' newtype wrapper to the tag type.
--
-- @
-- type 'Lens'  s t a b = 'Optic'  'A_Lens' 'NoIx' s t a b
-- type 'Lens'' s   a   = 'Optic'' 'A_Lens' 'NoIx' s   a
-- @
--
-- 'NoIx' as the second parameter to 'Optic' indicates that the optic is not
-- indexed.  See the "Indexed optics" section below in "Optics#indexed" for
-- further discussion of indexed optics.
--
-- The details of the internal implementation of 'Optic' are hidden behind an
-- abstraction boundary, so that the library can be used without needing to
-- think about the particular implementation choices.
--
--
-- === Specification of optics interfaces
--
-- Each different kind of optic is documented in a separate module describing
-- its abstract interface, in a standard format with at least /formation/,
-- /introduction/, /elimination/, and /well-formedness/ sections.  See "Optic
-- kinds" below in "Optics#optickinds" for a list of these modules.
--
-- * The __formation__ sections contain type definitions. For example
--   "Optics.Lens" defines:
--
--     @
--     -- Type synonym for a type-modifying lens.
--     type 'Lens' s t a b = 'Optic' 'A_Lens' 'NoIx' s t a b
--     @
--
-- * The __introduction__ sections describe the canonical way to construct each
--   particular optic. Continuing with a 'Lens' example:
--
--     @
--     -- Build a lens from a getter and a setter.
--     'lens' :: (s -> a) -> (s -> b -> t) -> 'Lens' s t a b
--     @
--
-- * Correspondingly, the __elimination__ sections show how you can destruct the
--   optic into the pieces from which it was constructed.
--
--     @
--     -- A 'Lens' is a 'Setter' and a 'Getter', therefore you can specialise types to obtain
--     'view' :: 'Lens' s t a b -> s -> a
--     'set'  :: 'Lens' s t a b -> b -> s -> t
--     @
--
-- * The __computation__ rules tie introduction and elimination forms
--   together. These rules are automatically fulfilled by the library (for
--   well-formed optics).
--
--     @
--     'view' ('lens' f g)   s ≡ f s
--     'set'  ('lens' f g) a s ≡ g s a
--     @
--
-- * The __well-formedness__ sections describe the laws that each optic should
--   obey.  As far as possible, all optics provided by the library are
--   well-formed, but in some cases this depends on invariants that cannot be
--   expressed in types.  Ill-formed optics /might/ behave differently from what
--   the computation rules specify.
--
--     For example, a 'Lens' should obey three laws, known as /GetPut/, /PutGet/
--     and /PutPut/.  See the "Optics.Lens" module for their definitions.  The
--     user of the 'lens' introduction form must ensure that these laws are
--     satisfied.
--
-- * Some optic kinds have __additional introduction forms__,
--   __additional elimination forms__ or __combinators__ sections, which give
--   alternative ways to create and use optics of that kind.  In principle these
--   are expressible in terms of the canonical introduction and elimination
--   rules.
--
-- * The __subtyping__ section gives the "tag type" (such as 'A_Lens'), which in
--   particular is accompanied by 'Is' instances that define the subtyping
--   relationship discussed in the following section.
--
--
-- === Subtyping
--
-- There is a subtyping relationship between optics, implemented using
-- typeclasses.  The 'Is' typeclass captures the property that one optic kind
-- can be used as another, and the 'castOptic' function can be used to
-- explicitly cast between optic kinds.  'Is' forms a partial order, represented
-- in the graph below.  For example, a lens can be used as a traversal, so there
-- are arrows from 'Lens' to 'Traversal' (via 'AffineTraversal') and there is an
-- instance of @'Is' 'A_Lens' 'A_Traversal'@.
--
-- Introduction forms (constructors) return a concrete optic kind, while
-- elimination forms (destructors) are generally polymorphic in the optic kind
-- they accept.  This means that it is not normally necessary to explicitly cast
-- between optic kinds.  For example, we have
--
-- @
-- 'view' :: 'Is' k 'A_Getter' => 'Optic'' k is s a -> s -> a
-- @
--
-- so 'view' can be used with isomorphisms or lenses, as these can be converted
-- to a 'Getter'.
--
-- If an explicit cast is needed, you can use 'castOptic'. This arises when you
-- use optics of different kinds in a context that requires them to have the
-- same type. For example @['folded', 'traversed']@ gives a type error (since
-- 'A_Traversal' is not 'A_Fold') but @['folded', 'castOptic' 'traversed']@
-- works.
--
-- The optic kind module (e.g. "Optics.Lens") does not list all ways to
-- construct or use particular the optic kind.  For example, since a 'Lens' is
-- also a 'Traversal', a 'Fold' etc, so you can use 'traverseOf', 'preview' and
-- many other combinators with lenses.
--
--
-- ==== Subtype hierarchy
--
-- This graph gives an overview of the optic kinds and their subtype
-- relationships:
--
-- <<diagrams/optics.png Optics hierarchy>>
--
-- In addition to the optic kinds included in the diagram, there are also
-- indexed variants such as 'IxLens', 'IxGetter', 'IxAffineTraversal',
-- 'IxTraversal', 'IxAffineFold', 'IxFold' and 'IxSetter'.  These are explained
-- in more detail in the "Indexed optics" section below in "Optics#indexed".
--
--
-- === Composition
--
-- Since /optics are not functions/, they cannot be composed with the ('.')
-- operator. Instead there is a separate composition operator ('%'). The
-- composition operator returns the common supertype of its arguments, or
-- generates a type error if the composition does not make sense.
--
-- The optic kind resulting from a composition is the least upper bound (join)
-- of the optic kinds being composed, if it exists.  The 'JoinKinds' class
-- computes the least upper bound given two optic kind tags.  For example the
-- constraint 'JoinKinds A_Lens A_Prism k' makes GHC infer that @k@ must be
-- 'An_AffineTraversal'.
--
-- The join does not exist for some pairs of optic kinds, which means that they
-- cannot be composed.  For example there is no optic kind above both 'Setter'
-- and 'Fold':
--
-- >>> :t mapped % folded
-- ...
-- ...A_Setter cannot be composed with A_Fold
-- ...
--
-- The ('Control.Category..') operator from "Control.Category" cannot be used to
-- compose optics either, because it would not support type-changing optics or
-- composing optics of different kinds.

-- $lens_comparison
--
-- The @lens@ package is the best known Haskell library for optics, and
-- established many of the foundations on which the @optics@ package builds (not
-- least in quite a bit of code having been directly ported).  It defines optics
-- based on the /van Laarhoven/ representation, where each optic kind is
-- introduced as a /transparent/ type synonym for a complex polymorphic type,
-- for example:
--
-- @
-- type Lens s t a b = forall f. 'Functor' f => (a -> f b) -> s -> f t
-- @
--
-- In contrast, @optics@ tries to preserve an abstraction boundary between the
-- interface of optics and their implementation.  Optic kinds are expressed
-- directly in the types, as 'Optic' is an /opaque/ newtype:
--
-- @
-- type 'Lens' s t a b = 'Optic' 'A_Lens' 'NoIx' s t a b
-- @
--
-- The choice of representation of 'Optic' is then an implementation detail, not
-- essential for understanding the library.  (In fact, @optics@ uses the
-- /profunctor/ representation rather than the /van Laarhoven/ representation;
-- this affects the optic kinds and operations that can be conveniently
-- supported, but not the essence of the design.)
--
-- Our design choice to use /opaque/ rather than /transparent/ abstractions
-- leads to various consequences, both positive and negative, which are explored
-- in the following subsections.
--
-- == Advantages of the opaque design
--
-- Since the interface is deliberately chosen rather than to some extent
-- determined by the implementation, we are free to choose a more restricted
-- interface where doing so leads to conceptual simplicity.  For example, in
-- @lens@, the 'view' function can be used with a 'Fold' provided the result
-- type has a 'Monoid' instance, and the multiple targets of the 'Fold' will be
-- combined monoidally.  This behaviour can be confusing, so in @optics@ a
-- 'Fold' cannot be silently used as a 'Getter', and we prefer to have 'view'
-- work on 'Getter's and define a separate 'foldOf' operator for use on
-- 'Fold's. (But the 'gview' function is available for users who may prefer
-- otherwise.)
--
-- In general, opaque abstractions lead to better results from type inference
-- (the optic kind is preserved in the inferred type):
--
--     >>> :t traversed % to not
--     traversed % to not
--       :: Traversable t => Optic A_Fold '[] (t Bool) (t Bool) Bool Bool
--
-- Error messages are domain-specific:
--
--     >>> set (to fst)
--     ...
--     ...A_Getter cannot be used as A_Setter
--     ...
--
-- Composing incompatible optics yields a sensible error:
--
--     >>> sets map % to not
--     ...
--     ...A_Setter cannot be composed with A_Getter...
--     ...
--
-- Since 'Optic' is a rank-1 type, it is easy to store optics in a
-- datastructure:
--
-- >>> :t [folded, backwards_ folded]
-- [folded, backwards_ folded] :: Foldable f => [Fold (f a) a]
--
-- It is possible to define aliases for optics without the monomorphism
-- restriction spoiling the fun:
--
-- >>> let { myoptic = _1; p = ('x','y') } in (view myoptic p, set myoptic 'c' p)
-- ('x',('c','y'))
--
-- Finally, having an abstract interface gives more freedom of choice in the
-- internal implementation.  If there is a compelling reason to switch to an
-- alternative representation, one can in principle do so without changing the
-- interface.
--
--
-- == Disadvantages of the opaque design
--
-- Since 'Optic' is a newtype, other libraries that wish to define optics must
-- depend upon its definition.  In contrast, with a transparent representation,
-- and since the van Laarhoven representations of lenses and traversals depend
-- only on definitions from @base@, it is possible for libraries to define them
-- without any extra library dependencies (although this does not hold for more
-- advanced optic kinds such as prisms or indexed optics).  To address this, the
-- present library is split into a package @optics-core@, which has a minimal
-- dependency footprint intended for use in libraries, and the
-- \"batteries-included\" @optics@ package for use in applications.
--
-- It is something of an amazing fact that the composition operator for
-- transparent optics is just function composition.  Moreover, since Haskell
-- uses ('.')  for function composition, @lens@ is able to support a pseudo-OOP
-- syntax.  In contrast, @optics@ must use a different composition operator
-- ('%').  'Optic' does not quite form a 'Control.Category.Category', thanks to
-- type-changing optics.
--
-- Rather than emerging naturally from the definitions, opportunities for
-- polymorphism have to be identified in advance and explicitly introduced using
-- type classes.  Similarly, the set of optic kinds and the subtyping
-- relationships between them must be fixed in advance, and cannot be added to
-- in downstream libraries.  Thus in a sense the opaque approach is more
-- restrictive than the transparent one. There are cases in @lens@ where the
-- types work out nicely and permit abstraction-breaking-but-convenient
-- shortcuts, such as applying a 'Traversal' as a 'traverse'-like function,
-- whereas @optics@ requires a call to 'traverseOf'.
--
--
-- == More specific differences
--
-- The sections above set out the major conceptual differences from the @lens@
-- package, and their advantages and disadvantages.  Some more specific design
-- differences, which may be useful for comparison or porting code between the
-- libraries.  This list is no doubt incomplete.
--
-- * The composition operator is ('%') rather than ('.') and is defined as
--   @infixl 9@ instead of @infixr 9@.
--
-- * Fewer operators are provided, and some of them are not exported from the
--   main "Optics" module. Import "Optics.State.Operators" if you want them.
--
-- * The 'view' function and corresponding ('Optics.Operators.^.') operator work
--   only for 'Getter's and have a more restricted type. The equivalent for
--   'Fold's is 'foldOf', and you can use 'preview' for
--   'AffineFold's. Alternatively you can use 'gview' which is more compatible
--   with @view@ from @lens@, but it uses a type class to choose between 'view',
--   'preview' and 'foldOf'.
--
-- * Indexed optics are rather different, as described in the "Indexed optics"
--   section below in "Optics#indexed".  All ordinary optics are
--   "index-preserving", so there is no separate notion of an index-preserving
--   optic.
--
-- * 'Each' provides indexed traversals.
--
-- * @firstOf@ from @lens@ is replaced by 'headOf'.
--
-- * @concatOf@ from @lens@ is omitted in favour of the more general 'foldOf'.
--
-- * 'set'' is a strict version of 'set', not 'set' for type-preserving optics.
--
-- * Numbered lenses for accessing fields of tuples positionally are provided
--   only up to '_9', rather than @_19@.
--
-- * There are four variants of @backwards@ for (indexed) 'Traversal's and
--   'Fold's: 'backwards', 'backwards_', 'ibackwards' and 'ibackwards_'.
--
-- * There is no @Traversal1@ and @Fold1@.
--
-- * There are affine variants of (indexed) traversals and folds
--   ('AffineTraversal', 'AffineFold', 'IxAffineTraversal' and 'IxAffineFold').
--   An affine optic targets at most one value.  Composing a 'Lens' with a
--   'Prism' produces an 'AffineTraversal', so for example @'matching' ('_1' '%'
--   '_Left')@ is well-typed.
--
-- * Functions 'ifiltered' and 'indices' are defined as optic combinators due to
--   restrictions of internal representation.
--
-- * We can't use 'traverse' as an optic directly.  Instead there is a
--   'Traversal' called 'traversed'.  Similarly 'traverseOf' must be used to
--   apply a 'Traversal', rather than simply using it as a function.
--
-- * The 're' combinator produces a different optic kind depending on the kind
--   of the input 'Iso', for example 'Review' reverses to 'Getter' while a
--   reversed 'Iso' is still an 'Iso'.  Thus there is no separate @from@
--   combinator for reversing 'Iso's.
--
-- * 'singular' ('isingular' for indexed optics) doesn't produce a partial lens
--   that might fail with a runtime error, but an affine traversal.
--
-- * '<>' cannot be used to combine 'Fold's, so 'summing' should be used instead
--   (see the "Monoid structures" section below in "Optics#monoids").


-- $otherresources
--
-- === Talks
--
-- * (2020-10) <https://skillsmatter.com/skillscasts/14906-user-friendly-optics User Friendly Optics> - a talk about the @optics@ library in comparison to the @lens@ library by Andrzej Rybczak
--
-- * (2020-06) <https://www.youtube.com/watch?v=geV8F59q48E Basic optics: lenses, prisms, and traversals> - an introductory talk about this library by Alejandro Serrano
--
-- * (2018-10) <https://skillsmatter.com/skillscasts/12360-profunctors-and-data-accessors Profunctors and Data Accessors> - a talk on basics of profunctors and how they relate to data accessors such as lenses, prisms and traversals by Andrzej Rybczak
--
-- * (2017-10) <https://skillsmatter.com/skillscasts/10692-through-a-glass-abstractly-lenses-and-the-power-of-abstraction Through a Glass, Abstractly: Lenses and the Power of Abstraction> - a talk on the principles behind this library with <https://github.com/well-typed/optics/raw/master/Talk.pdf accompanying slides> by Adam Gundry (but note that the design details of @optics@ have changed substantially since this talk was given)
--
-- === Articles
--
-- * (2020-01) <https://oleg.fi/gists/posts/2020-01-25-case-study-migration-from-lens-to-optics.html Case study: migrating from lens to optics> - a blog post by Oleg Grenrus, potentially useful if you wish to migrate an existing codebase to @optics@ from @lens@
--
-- * (2017-04) <https://oleg.fi/gists/posts/2017-04-18-glassery.html Glassery> and <https://oleg.fi/gists/posts/2017-04-26-indexed-poptics.html Indexed Profunctor optics> - blog posts by Oleg Grenrus on the internal representations used by this library
--
-- * (2017-03) <https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf Profunctor Optics: Modular Data Accessors> - a paper by Matthew Pickering, Jeremy Gibbons and Nicolas Wu
--
-- === Libraries
--
-- * The <https://hackage.haskell.org/package/lens lens> package by Edward Kmett and contributors
--


-- $basicusage
--
-- To get started, you can just add @optics@ as a dependency to your @.cabal@
-- file, and then:
--
-- @
-- import "Optics"
-- @
--
-- If you are writing a library for which it is important to keep the dependency
-- footprint minimal, you may wish to depend upon @optics-core@ instead (and
-- perhaps @optics-extra@ or @optics-th@), and then:
--
-- @
-- import "Optics.Core"
-- @

-- $indexed #indexed#
--
-- The @optics@ library also provides indexed optics, which provide
-- an additional /index/ value in mappings:
--
-- @
-- 'over'  :: 'Setter'     s t a b -> (a -> b)      -> s -> t
-- 'iover' :: 'IxSetter' i s t a b -> (i -> a -> b) -> s -> t
-- @
--
-- Note that there aren't any laws about indices.
-- Especially in compositions the same index may occur multiple times.
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
-- >>> itoListOf ifolded "foo"
-- [(0,'f'),(1,'o'),(2,'o')]
--
-- But there is also a combinator 'noIx' to explicitly erase indices:
--
-- >>> :t (ifolded % simple)
-- (ifolded % simple)
--   :: FoldableWithIndex i f => Optic A_Fold '[i] (f b) (f b) b b
--
-- >>> :t noIx (ifolded % simple)
-- noIx (ifolded % simple)
--   :: FoldableWithIndex i f => Optic A_Fold NoIx (f b) (f b) b b
--
-- @
-- λ> :t noIx (ifolded % ifolded)
-- noIx (ifolded % ifolded)
--   :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
--      Optic A_Fold NoIx (f1 (f2 b)) (f1 (f2 b)) b b
-- @
--
-- As the example above illustrates, regular and indexed optics have the same
-- tag in the first parameter of 'Optic', in this case 'A_Fold'.  Regular optics
-- simply don't have any indices.  The provided type aliases 'IxLens',
-- 'IxGetter', 'IxAffineTraversal', 'IxAffineFold', 'IxTraversal', 'IxFold' and
-- 'IxSetter' are variants with a single index. In general, the second parameter
-- of the 'Optic' newtype is a type-level list of indices, which will typically
-- be 'NoIx' (the empty index list) or @('WithIx' i)@ (a singleton list).
--
-- When two optics are composed with ('%'), the index lists are concatenated.
-- Thus composing an unindexed optic with an indexed optic preserves the
-- indices, or composing two indexed optics retains both indices:
--
-- @
-- λ> :t (ifolded % ifolded)
-- (ifolded % ifolded)
--   :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
--      Optic A_Fold '[i1, i2] (f1 (f2 b)) (f1 (f2 b)) b b
-- @
--
-- In order to use such an optic, it is necessary to flatten the indices into a
-- single index using 'icompose' or a similar function:
--
-- @
-- λ> :t icompose (,) (ifolded % ifolded)
-- icompose (,) (ifolded % ifolded)
--   :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
--      Optic A_Fold (WithIx (i1, i2)) (f1 (f2 b)) (f1 (f2 b)) b b
-- @
--
-- For example:
--
-- >>> itoListOf (icompose (,) (ifolded % ifolded)) [['a','b'], ['c', 'd']]
-- [((0,0),'a'),((0,1),'b'),((1,0),'c'),((1,1),'d')]
--
-- Alternatively, you can use one of the ('<%') or ('%>') operators to compose
-- indexed optics and pick the index to retain, or the ('<%>') operator to
-- retain a pair of indices:
--
-- >>> itoListOf (ifolded <% ifolded) [['a','b'], ['c', 'd']]
-- [(0,'a'),(0,'b'),(1,'c'),(1,'d')]
--
-- >>> itoListOf (ifolded %> ifolded) [['a','b'], ['c', 'd']]
-- [(0,'a'),(1,'b'),(0,'c'),(1,'d')]
--
-- >>> itoListOf (ifolded <%> ifolded) [['a','b'], ['c', 'd']]
-- [((0,0),'a'),((0,1),'b'),((1,0),'c'),((1,1),'d')]
--
-- In the diagram below, the optics hierarchy is amended with these (singly) indexed variants (in blue).
-- Orange arrows mean
-- "can be used as one, assuming it's composed with any optic below the
-- orange arrow first". For example. '_1' is not an indexed fold, but
-- @'itraversed' % '_1'@ is, because it's an indexed traversal, so it's
-- also an indexed fold.
--
-- >>> let fst' = _1 :: Lens (a, c) (b, c) a b
-- >>> :t fst' % itraversed
-- fst' % itraversed
--   :: TraversableWithIndex i f =>
--      Optic A_Traversal '[i] (f a, c) (f b, c) a b
--
-- <<diagrams/indexedoptics.png Indexed Optics>>


-- $monoids
--
-- There are two ways to combine (possibly indexed) folds, traversals and
-- related optics with the same outer and inner types:
--
-- * Visit all the targets of the first optic, then all the targets of the
--   second optic.  This makes sense for folds ('summing' or 'isumming') and
--   traversals ('adjoin' or 'iadjoin'), provided in the latter case that the
--   targets are disjoint.
--
-- * Visit the targets of the first optic if there are any, or if not, visit the
--   targets of the second optic.  This makes sense for folds ('failing' or
--   'ifailing') and affine folds ('afailing' or 'iafailing').
--
-- These operations form monoid structures on the appropriate optic kinds, with
-- the identity element 'ignored', which visits no targets.
--
-- There is no 'Semigroup' or 'Monoid' instance for 'Optic', because there is
-- not a unique choice of monoid to use, and the ('<>') operator could not be
-- used to combine optics of different kinds. When porting code from @lens@ that
-- uses ('<>') to combine folds, use 'summing' instead.


-- $cheatsheet
--
-- The following table summarizes the key optic kinds and their combinators.
-- It is based on a [similar table for the lens
-- package](https://github.com/ekmett/lens/wiki/operators).
--
-- A 'Lens' can be used as a 'Getter', 'Setter', 'Fold' and 'Traversal'.
--
-- +-------------------+---------------+---------------------------------------------------+
-- |Combinator         |Indexed        |Notes                                              |
-- +===================+===============+===================================================+
-- |                                                                                       |
-- +-------------------+---------------+---------------------------------------------------+
-- |__Getters__                                                                            |
-- +-------------------+---------------+---------------------------------------------------+
-- |'to'               |'ito'          |Build a 'Getter' / 'IxGetter' from a plain         |
-- |                   |               |function.                                          |
-- +-------------------+---------------+---------------------------------------------------+
-- |'view' / '^.'      |'iview'        |View a single target.                              |
-- +-------------------+---------------+---------------------------------------------------+
-- |'views'            |'iviews'       |View after applying a function.                    |
-- +-------------------+---------------+---------------------------------------------------+
-- |                                                                                       |
-- +-------------------+---------------+---------------------------------------------------+
-- |__Setters__                                                                            |
-- +-------------------+---------------+---------------------------------------------------+
-- |'sets'             |'isets'        |Build a 'Setter' / 'IxSetter' from an update       |
-- |                   |               |function.                                          |
-- +-------------------+---------------+---------------------------------------------------+
-- |'mapped'           |'imapped'      |Build a 'Setter' from the 'Functor' class, or an   |
-- |                   |               |'IxSetter' from 'FunctorWithIndex'.                |
-- +-------------------+---------------+---------------------------------------------------+
-- |'set' / '.~'       |'iset'         |Replace target(s) with value.                      |
-- +-------------------+---------------+---------------------------------------------------+
-- |'over' / '%~'      |'iover'        |Modify target(s) by applying a function.           |
-- +-------------------+---------------+---------------------------------------------------+
-- |                                                                                       |
-- +-------------------+---------------+---------------------------------------------------+
-- |__Folds__          |                                                                   |
-- +-------------------+---------------+---------------------------------------------------+
-- |'folded'           | 'ifolded'     |Build a 'Fold' from the 'Foldable' class, or an    |
-- |                   |               |'IxFold' from 'FoldableWithIndex'.                 |
-- +-------------------+---------------+---------------------------------------------------+
-- |'toListOf' / '^..' |'itoListOf'    |Return a list of the target(s).                    |
-- +-------------------+---------------+---------------------------------------------------+
-- |                                                                                       |
-- +-------------------+---------------+---------------------------------------------------+
-- |__AffineFolds__                                                                        |
-- +-------------------+---------------+---------------------------------------------------+
-- | 'afolding'        |'iafolding'    |Build an 'AffineFold' / 'IxAffineFold' from a      |
-- |                   |               |partial function.                                  |
-- +-------------------+---------------+---------------------------------------------------+
-- |'preview' / '^?'   |'ipreview'     |Match the target or return 'Nothing'.              |
-- +-------------------+---------------+---------------------------------------------------+
-- |'previews'         |'ipreviews'    |Preview after applying a function.                 |
-- +-------------------+---------------+---------------------------------------------------+
-- |                                                                                       |
-- +-------------------+---------------+---------------------------------------------------+
-- |__Traversals__                                                                         |
-- +-------------------+---------------+---------------------------------------------------+
-- |'traversed'        |'itraversed'   |Build a 'Traversal' from the 'Traversable' class,  |
-- |                   |               |or an 'IxTraversal' from 'TraversableWithIndex'.   |
-- +-------------------+---------------+---------------------------------------------------+
-- |'traverseOf'       |'itraverseOf'  |Update target(s) with an 'Applicative'.            |
-- +-------------------+---------------+---------------------------------------------------+
-- |                                                                                       |
-- +-------------------+---------------+---------------------------------------------------+
-- |__Prisms__                                                                             |
-- +-------------------+---------------+---------------------------------------------------+
-- |'prism'            |               |Build a 'Prism' from a constructor and matcher.    |
-- +-------------------+---------------+---------------------------------------------------+
-- |'review' / '#'     |               |Use a 'Prism' to construct the sum type.           |
-- +-------------------+---------------+---------------------------------------------------+
--
-- For setting/modifying using a 'Setter', a variety of combinators are available
-- in "Optics.State" and "Optics.State.Operators".  The latter are not exported
-- by the main "Optics" module, so must be imported explicitly.
--
-- +--------------+-----------------+-------------------------------------------+------------------------------+-------------------------------+-------------------------------------------+
-- | Lazy         | Strict          | Stateful                                  | Stateful returning new value | Stateful returning old value  | Notes                                     |
-- +==============+=================+===========================================+==============================+===============================+===========================================+
-- |'set' / '.~'  | 'set'' / '!~'   | 'assign' / 'Optics.State.Operators..='    | 'Optics.State.Operators.<.=' | 'Optics.State.Operators.<<.=' | Replace target(s) with value.             |
-- +--------------+-----------------+-------------------------------------------+------------------------------+-------------------------------+-------------------------------------------+
-- |'over' / '%~' | 'over'' / '%!~' | 'modifying' / 'Optics.State.Operators.%=' | 'Optics.State.Operators.<%=' | 'Optics.State.Operators.<<%=' | Modify target(s) by applying a function.  |
-- +--------------+-----------------+-------------------------------------------+------------------------------+-------------------------------+-------------------------------------------+
-- | '?~'         | '?!~'           | 'Optics.State.Operators.?='               | 'Optics.State.Operators.<?=' | 'Optics.State.Operators.<<?=' | Replace target(s) with 'Just' a value.    |
-- +--------------+-----------------+-------------------------------------------+------------------------------+-------------------------------+-------------------------------------------+

-- $setup
-- >>> import Control.Monad.Reader
-- >>> import Control.Monad.State
-- >>> import Data.Functor.Identity
-- >>> import qualified Data.IntSet as IntSet
-- >>> import qualified Data.Map as Map
-- >>> import Optics.State.Operators
