-- |
-- Module:  Data.List.Optics
-- Description: Traversals for manipulating parts of a list.
--
-- Additional optics for manipulating lists are present more generically in this
-- package.
--
-- The 'Optics.At.Core.Ixed' class allows traversing the element at a specific
-- list index.
--
-- >>> [0..10] ^? ix 4
-- Just 4
--
-- >>> [0..5] & ix 4 .~ 2
-- [0,1,2,3,2,5]
--
-- >>> [0..10] ^? ix 14
-- Nothing
--
-- >>> [0..5] & ix 14 .~ 2
-- [0,1,2,3,4,5]
--
-- The 'Optics.Cons.Core.Cons' and 'Optics.Empty.Core.AsEmpty' classes provide
-- 'Optics.Prism.Prism's for list constructors.
--
-- >>> [1..10] ^? _Cons
-- Just (1,[2,3,4,5,6,7,8,9,10])
--
-- >>> [] ^? _Cons
-- Nothing
--
-- >>> [] ^? _Empty
-- Just ()
--
-- >>> _Cons # (1, _Empty # ()) :: [Int]
-- [1]
--
-- Additionally, 'Optics.Cons.Core.Snoc' provides a 'Optics.Prism.Prism' for
-- accessing the end of a list. Note that this 'Optics.Prism.Prism' always will
-- need to traverse the whole list.
--
-- >>> [1..5] ^? _Snoc
-- Just ([1,2,3,4],5)
--
-- >>> _Snoc # ([1,2],5)
-- [1,2,5]
--
-- Finally, it's possible to traverse, fold over, and map over index-value pairs
-- thanks to instances of 'Optics.Indexed.Core.TraversableWithIndex',
-- 'Optics.Indexed.Core.FoldableWithIndex', and
-- 'Optics.Indexed.Core.FunctorWithIndex'.
--
-- >>> imap (,) "Hello"
-- [(0,'H'),(1,'e'),(2,'l'),(3,'l'),(4,'o')]
--
-- >>> ifoldMap replicate "Hello"
-- "ellllloooo"
--
-- >>> itraverse_ (curry print) "Hello"
-- (0,'H')
-- (1,'e')
-- (2,'l')
-- (3,'l')
-- (4,'o')
--
----------------------------------------------------------------------------
module Data.List.Optics
  ( prefixed
  , suffixed
  ) where

import Control.Monad (guard)
import Data.List

import Optics.Prism

-- | A 'Prism' stripping a prefix from a list when used as a
-- 'Optics.Traversal.Traversal', or prepending that prefix when run backwards:
--
-- >>> "preview" ^? prefixed "pre"
-- Just "view"
--
-- >>> "review" ^? prefixed "pre"
-- Nothing
--
-- >>> prefixed "pre" # "amble"
-- "preamble"
prefixed :: Eq a => [a] -> Prism' [a] [a]
prefixed ps = prism' (ps ++) (stripPrefix ps)
{-# INLINE prefixed #-}

-- | A 'Prism' stripping a suffix from a list when used as a
-- 'Optics.Traversal.Traversal', or appending that suffix when run backwards:
--
-- >>> "review" ^? suffixed "view"
-- Just "re"
--
-- >>> "review" ^? suffixed "tire"
-- Nothing
--
-- >>> suffixed ".o" # "hello"
-- "hello.o"
suffixed :: Eq a => [a] -> Prism' [a] [a]
suffixed qs = prism' (++ qs) (stripSuffix qs)
{-# INLINE suffixed #-}

----------------------------------------
-- Internal

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix qs xs0 = go xs0 zs
  where
    zs = drp qs xs0
    drp (_:ps) (_:xs) = drp ps xs
    drp [] xs = xs
    drp _  [] = []
    go (_:xs) (_:ys) = go xs ys
    go xs [] = zipWith const xs0 zs <$ guard (xs == qs)
    go [] _  = Nothing -- impossible
{-# INLINE stripSuffix #-}

-- $setup
-- >>> import Optics.Core
