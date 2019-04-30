-- |
--
-- Module: Optics
-- Description: The main module, usually you only need to import this one.
--
-- This library makes it possible to define and use 'Lens'es, 'Traversal's,
-- 'Prism's and other /optics/, using an abstract interface.  These are
-- frequently introduced as type synonyms for complex polymorphic types, for
-- example the @lens@ library defines:
--
-- @
-- type Lens s t a b = forall f. 'Functor' f => (a -> f b) -> s -> f t
-- @
--
-- In contrast, this library defines a newtype wrapper 'Optic' to capture the
-- general concept.  This newtype is indexed by the particular optic kind
-- (e.g. 'A_Lens'), so that while 'Lens' is still a type synonym, its definition
-- is much simpler:
--
-- @
-- type 'Lens' s t a b = 'Optic' 'A_Lens' 'NoIx' s t a b
-- @
--
-- The details of the internal implementation of 'Optic' are hidden behind an
-- abstraction boundary, so that the library can be used without needing to
-- think about the particular implementation choices.  (In fact, @optics@ uses
-- the \"profunctor\" representation rather than the \"van Laarhoven\"
-- representation mentioned above.)
--
module Optics
  (
  -- * Introduction
  -- $introduction

  -- ** Advantages
  -- $advantages

  -- ** Disadvantages
  -- $disadvantages

  -- ** Differences from @lens@
  -- $differences

  -- ** Other resources
  -- $otherresources

  -- * Basic usage
  -- $basicusage

  -- * Core definitions and subtyping
  -- $coredefinitions
    module Optics.Optic

  -- * Optic kinds
  -- $optickinds
  , module O

  -- * Indexed optics
  -- $indexed
  , module Optics.Indexed

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

  -- | A 'Traversal' for a (potentially monomorphic) container.
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
  -- The 'LensyReview' and 'PrismaticGetter' optic kinds are essentially
  -- equivalent to 'Review' and 'Getter' respectively, but are present so that
  -- @'re' . 're'@ does not change the optic kind.
  --
  -- <<reoptics.png Reversed Optics>>
  --
  , module Optics.Re

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
import Optics.Setter                         as O
import Optics.Review                         as O
import Optics.PrismaticGetter                as O
import Optics.Prism                          as O
import Optics.LensyReview                    as O
import Optics.Lens                           as O
import Optics.IxTraversal                    as O
import Optics.IxSetter                       as O
import Optics.IxFold                         as O
import Optics.IxAffineTraversal              as O
import Optics.IxAffineFold                   as O
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

-- $introduction
--
-- A key principle behind this library is the belief that optics are useful as
-- an abstract concept, and that the purpose of types is to capture abstract
-- concepts and make them useful.  The programmer using optics should be able to
-- think in terms of the abstract interface, rather than the details of the
-- implementation, and implementation choices should not dictate the interface.
--
-- Each optic kind has a module describing its abstract interface (e.g. see
-- "Optics.Lens"). See "Optic kinds" below in "Optics#optickinds" for a
-- discussion of the standard format in which these interfaces are described.
--
-- There is a subtyping relationship between optics, which is implemented using
-- typeclasses.  In particular, the 'Is' typeclass captures the property that
-- one optic kind can be used as another, for example the @'Is' 'A_Lens'
-- 'A_Traversal'@ instance means that lenses can be used as traversals.
-- Introduction forms (constructors) return a concrete optic kind, while
-- elimination forms are generally polymorphic in the optic kind they accept.
-- See "Core definitions and subtyping" below in "Optics#coredefinitions" for
-- more details.
--
-- For example,
--
-- @
-- 'view' :: 'Is' k 'A_Getter' => 'Optic'' k is s a -> s -> a
-- @
--
-- so 'view' can be used with lenses, traverals or any other optic kind that can
-- be converted to a 'Getter'.  Note, however, that a 'Fold' cannot be
-- implicitly converted to a 'Getter'.  Instead there is a separate eliminator
--
-- @
-- 'foldOf' :: ('Is' k 'A_Fold', 'Monoid' a) => 'Optic'' k is s a -> s -> a
-- @
--
-- which combines the (possibly multiple) results of the 'Fold' using the
-- 'Monoid' instance.
--
-- Since /optics are not functions/, they cannot be composed with the ('.')
-- operator. Instead there is a separate composition operator ('%'). The
-- composition operator returns the common supertype of its arguments, or
-- generates a type error if the composition does not make sense.


-- $advantages
--
-- In general, this leads to better results from type inference (the optic kind
-- is preserved in the inferred type):
--
--     >>> :t traversed % to not
--     traversed % to not
--       :: Traversable t => Optic A_Fold NoIx (t Bool) (t Bool) Bool Bool
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
--     ...A_Setter cannot be composed with A_Getter
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
-- Since the interface is chosen rather than predetermined by the
-- implementation, we are free to choose a more restricted interface where doing
-- so leads to conceptual simplicity.  Thus we prefer separate 'view' and
-- 'foldOf' operators rather than having 'view' unexpectedly introduce a
-- 'Monoid' constraint when used with a 'Fold'.
--
-- Finally, having an abstract interface gives more freedom of choice in the
-- internal implementation.  If there is a compelling reason to switch to an
-- alternative representation, one can in principle do so without changing the
-- interface.


-- $disadvantages
--
-- Since 'Optic' is a newtype, it is not possible for other libraries to define
-- optics without depending on the definition of 'Optic'.  Thus the library is
-- split into a package @optics-core@, which has a minimal dependency footprint,
-- and the \"batteries-included\" @optics@ package for use in applications.
-- (Since the van Laarhoven representations of lenses and traversals depend only
-- on definitions from @base@, it is possible for libraries to define them
-- without any extra dependencies, although this does not hold for more advanced
-- optic kinds such as prisms or indexed optics).
--
-- Rather than emerging naturally from the definitions, opportunities for
-- polymorphism have to be identified in advance and explicitly introduced using
-- type classes.  Similarly, the set of optic kinds and the subtyping
-- relationships between them must be fixed in advance, and cannot be added to
-- in downstream libraries.


-- $differences
--
-- The sections above set out the major conceptual differences from the @lens@
-- package. Some more specific design differences:
--
-- * The composition operator is ('%') rather than ('.') and is defined as
-- * @infixl 9@ instead of @infixr 9@.
--
-- * Fewer operators are provided, and none of operators are exported from the
--   main "Optics" module. Import "Optics.Operators" or "Optics.Operators.State"
--   if you want them.
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
-- * There is no @IndexedLens@, @IndexedGetter@, @Traversal1@ nor @Fold1@.
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
--   of the input 'Iso', for example 'Prism' reverses to 'Getter' while a
--   reversed 'Iso' is still an 'Iso'.  Thus there is no separate @from@
--   combinator for reversing 'Iso's.


-- $otherresources
--
-- * <https://skillsmatter.com/skillscasts/10692-through-a-glass-abstractly-lenses-and-the-power-of-abstraction Through a Glass, Abstractly: Lenses and the Power of Abstraction> a talk on the principles behind this library with <https://github.com/well-typed/optics/raw/master/Talk.pdf accompanying slides> by Adam Gundry
-- * <https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf Profunctor Optics: Modular Data Accessors> a paper by Matthew Pickering, Jeremy Gibbons and Nicolas Wu
-- * <http://oleg.fi/gists/posts/2017-04-26-indexed-poptics.html Indexed Profunctor optics> and <http://oleg.fi/gists/posts/2017-04-18-glassery.html Glassery>, blog posts by Oleg Grenrus


-- $basicusage
--
-- To get started, you can just
--
-- @
-- import "Optics"
-- @
--
-- and if you prefer to use operators
--
-- @
-- import "Optics.Operators"
-- import "Optics.Operators.State"
-- @


-- $coredefinitions #coredefinitions#
--
-- The "Optics.Optic" module provides core definitions:
--
-- * an opaque 'Optic' type, which is parameterised over a type representing an
--   optic kind (instantiated with tag types such as 'A_Lens');
--
-- * the optic composition operator ('%');
--
-- * the subtyping relation 'Is' with an accompanying 'castOptic' function to
--   convert an optic kind;
--
-- * the 'Join' operation used to find the optic kind resulting from a
--   composition.
--
-- Each optic kind is identified by a "tag type" (such as 'A_Lens'), which is an
-- empty data type.  The type of the actual optics (such as 'Lens') is obtained
-- by applying 'Optic' to the tag type.
--
-- The graph below represents the 'Is' partial order.  For example, a lens can
-- be used as a traversal, so there are arrows from 'Lens' to 'Traversal' (via
-- 'AffineTraversal') and there is an instance of @'Is' 'A_Lens' 'A_Traversal'@.
--
-- <<optics.png Optics hierarchy>>
--
-- The optic kind resulting from a composition is the least upper bound (join)
-- of the optic kinds being composed, if it exists.  The 'Join' type family
-- computes the least upper bound given two optic kind tags.  For example the
-- 'Join' of a 'Lens' and a 'Prism' is an 'AffineTraversal'.
--
-- >>> :kind! Join A_Lens A_Prism
-- Join A_Lens A_Prism :: *
-- = An_AffineTraversal
--
-- The join does not exist for some pairs of optic kinds, which means that they
-- cannot be composed.  For example there is no optic kind above both 'Setter'
-- and 'Fold':
--
-- >>> :kind! Join A_Setter A_Fold
-- Join A_Setter A_Fold :: *
-- = (TypeError ...)
--
-- >>> :t mapped % folded
-- ...
-- ...A_Setter cannot be composed with A_Fold
-- ...
--
-- In addition to the optic kinds described above, there are also indexed
-- variants, namely 'IxAffineTraversal', 'IxTraversal', 'IxAffineFold', 'IxFold'
-- and 'IxSetter'.  These are explained in more detail in the "Indexed optics"
-- section below in "Optics#indexed".


-- $optickinds #optickinds#
--
-- There are 16 different kinds of optics, each documented in a separate
-- module.  Each optic module documentation has /formation/, /introduction/,
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
-- >>> :t noIx (ifolded % ifolded)
-- noIx (ifolded % ifolded)
--   :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
--      Optic A_Fold NoIx (f1 (f2 b)) (f1 (f2 b)) b b
--
-- As the example above illustrates, regular and indexed optics have the same
-- tag in the first parameter of 'Optic', in this case 'A_Fold'.  Regular optics
-- simply don't have any indices.  The provided type aliases 'IxFold',
-- 'IxSetter' and 'IxTraversal' are variants with a single index. In general,
-- the second parameter of the 'Optic' newtype is a type-level list of indices,
-- which will typically be 'NoIx' (the empty index list) or @('WithIx' i)@ (a
-- singleton list).
--
-- When two optics are composed with ('%'), the index lists are concatenated.
-- Thus composing an unindexed optic with an indexed optic preserves the
-- indices, or composing two indexed optics retains both indices:
--
-- >>> :t (ifolded % ifolded)
-- (ifolded % ifolded)
--  :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
--     Optic A_Fold '[i1, i2] (f1 (f2 b)) (f1 (f2 b)) b b
--
-- In order to use such an optic, it is necessary to flatten the indices into a
-- single index using 'icompose' or a similar function:
--
-- >>> :t icompose (,) (ifolded % ifolded)
-- icompose (,) (ifolded % ifolded)
--  :: (FoldableWithIndex i1 f1, FoldableWithIndex i2 f2) =>
--     Optic A_Fold (WithIx (i1, i2)) (f1 (f2 b)) (f1 (f2 b)) b b
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


-- $setup
-- >>> import Control.Monad.Trans.Reader
-- >>> import Control.Monad.Trans.State
-- >>> import Data.Functor.Identity
-- >>> import Optics.Operators
-- >>> import Optics.Operators.State
-- >>> import qualified Data.IntSet as IntSet
-- >>> import qualified Data.Map as Map
--
