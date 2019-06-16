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
  ( Optic
  , Optic'

  -- * Subtyping
  , castOptic
  , Is
  , Join

  -- * Composition
  , (%)
  , (%%)
  , (%&)

  -- * Labels
  , LabelOptic(..)
  , LabelOptic'

  -- * Indexed optics
  , NoIx
  , WithIx
  , Append
  , NonEmptyIndices
  , HasSingleIndex
  , AcceptsEmptyIndices

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
