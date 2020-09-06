{-# LANGUAGE CPP #-}
-- |
-- Module: Optics.Optic
-- Description: Common abstraction for all kinds of optics.
--
-- This module provides core definitions:
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
-- empty data type.  The type of the actual optics (such as 'Optics.Lens.Lens')
-- is obtained by applying 'Optic' to the tag type.
--
-- See the @Optics@ module in the main @optics@ package for overview
-- documentation.
--
module Optics.Optic
  ( OpticKind
  , Optic
  , Optic'

  -- * Subtyping
  , castOptic
  , Is
  , Join

  -- * Composition
  -- | The usual operator for composing optics is ('%'), which allows different
  -- optic kinds to be composed, automatically calculating the resulting optic
  -- kind using 'Join'.
  --
  -- The ('.') function composition operator cannot be used to compose optics,
  -- because /optics are not functions/.  The ('Control.Category..') operator
  -- from "Control.Category" cannot be used either, because it would not support
  -- type-changing optics or composing optics of different kinds.
  , (%)
  , (%%)
  , (%&)

  -- * Monoid structures
  -- | 'Optics.Fold.Fold'-like optics admit various monoid structures (e.g. see
  -- "Optics.Fold#monoids").  There is no 'Semigroup' or 'Monoid' instance for
  -- 'Optic', however, because there is not a unique choice of monoid to use,
  -- and the ('<>') operator could not be used to combine optics of different
  -- kinds.

  -- * Indexed optics
  -- | See the "Indexed optics" section of the overview documentation in the
  -- @Optics@ module of the main @optics@ package for more details on indexed
  -- optics.
  , IxList
  , NoIx
  , WithIx
  , Append
  , AppendIndices
  , NonEmptyIndices
  , HasSingleIndex
  , AcceptsEmptyIndices
  , Curry
  , CurryCompose

    -- * Base re-exports
  , (&)
  , (<&>)
  )
  where

import Data.Function

import Optics.Internal.Indexed
import Optics.Internal.Optic

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#else
-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
#endif
