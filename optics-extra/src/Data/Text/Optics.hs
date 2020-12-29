{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module: Data.Text.Optics
-- Description: Optics for working with strict or lazy 'Text'.
--
-- This module provides 'Iso's for converting strict or lazy 'Text' to or from a
-- 'String' or 'Builder', and an 'IxTraversal' for traversing the individual
-- characters of a 'Text'.
--
-- The same combinators support both strict and lazy text using the 'IsText'
-- typeclass.  You can import "Data.Text.Strict.Optics" or
-- "Data.Text.Lazy.Optics" instead if you prefer monomorphic versions.
--
module Data.Text.Optics
  ( IsText(..)
  , unpacked
  , _Text
  , pattern Text
  ) where

import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as B

import Optics.Core
import qualified Data.Text.Lazy.Optics as Lazy
import qualified Data.Text.Strict.Optics as Strict

-- | Traversals for strict or lazy 'Text'
class IsText t where
  -- | This isomorphism can be used to 'pack' (or 'unpack') strict or lazy
  -- 'Text'.
  --
  -- @
  -- 'pack' x ≡ x 'Optics.Operators.^.' 'packed'
  -- 'unpack' x ≡ x 'Optics.Operators.^.' 're' 'packed'
  -- 'packed' ≡ 're' 'unpacked'
  -- @
  packed :: Iso' String t

  -- | Convert between strict or lazy 'Text' and a 'Builder'.
  --
  -- @
  -- 'fromText' x ≡ x 'Optics.Operators.^.' 'builder'
  -- @
  builder :: Iso' t B.Builder

  -- | Traverse the individual characters in strict or lazy 'Text'.
  --
  -- @
  -- 'text' = 'unpacked' . 'traversed'
  -- @
  text :: IxTraversal' Int t Char
  text = unpacked % itraversed
  {-# INLINE text #-}

instance IsText String where
  packed  = iso id id
  text    = itraversed
  builder = Lazy.packed % builder
  {-# INLINE packed #-}
  {-# INLINE text #-}
  {-# INLINE builder #-}

-- | This isomorphism can be used to 'unpack' (or 'pack') both strict or lazy
-- 'Text'.
--
-- @
-- 'unpack' x ≡ x 'Optics.Operators.^.' 'unpacked'
-- 'pack' x ≡ x 'Optics.Operators.^.' 're' 'unpacked'
-- @
--
-- This 'Iso' is provided for notational convenience rather than out of great
-- need, since
--
-- @
-- 'unpacked' ≡ 're' 'packed'
-- @
--
unpacked :: IsText t => Iso' t String
unpacked = re packed
{-# INLINE unpacked #-}

-- | This is an alias for 'unpacked' that makes it clearer how to use it with
-- @('Optics.Operators.#')@.
--
-- @
-- '_Text' = 're' 'packed'
-- @
--
-- >>> _Text # "hello" :: Strict.Text
-- "hello"
_Text :: IsText t => Iso' t String
_Text = re packed
{-# INLINE _Text #-}

pattern Text :: IsText t => String -> t
pattern Text a <- (view _Text -> a) where
  Text a = review _Text a

instance IsText Strict.Text where
  packed  = Strict.packed
  builder = Strict.builder
  text    = Strict.text
  {-# INLINE packed #-}
  {-# INLINE builder #-}
  {-# INLINE text #-}

instance IsText Lazy.Text where
  packed  = Lazy.packed
  builder = Lazy.builder
  text    = Lazy.text
  {-# INLINE packed #-}
  {-# INLINE builder #-}
  {-# INLINE text #-}
