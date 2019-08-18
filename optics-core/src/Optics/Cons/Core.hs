-- |
-- Module: Optics.Cons.Core
-- Description: Optics to access the left or right element of a container.
--
-- This module defines the 'Cons' and 'Snoc' classes, which provide 'Prism's for
-- the leftmost and rightmost elements of a container, respectively.
--
-- Note that orphan instances for these classes are defined in the @Optics.Cons@
-- module from @optics-extra@, so if you are not simply depending on @optics@
-- you may wish to import that module instead.
--
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Optics.Cons.Core
  (
  -- * Cons
    Cons(..)
  , (<|)
  , cons
  , uncons
  , _head, _tail
  , pattern (:<)
  -- * Snoc
  , Snoc(..)
  , (|>)
  , snoc
  , unsnoc
  , _init, _last
  , pattern (:>)
  ) where

import Control.Applicative (ZipList(..))
import Data.Coerce
import Data.Sequence hiding ((<|), (|>), (:<), (:>))
import qualified Data.Sequence as Seq

import Data.Tuple.Optics
import Optics.AffineFold
import Optics.AffineTraversal
import Optics.Coerce
import Optics.Prism
import Optics.Review

infixr 5 <|, `cons`
infixl 5 |>, `snoc`

-- | Pattern synonym for matching on the leftmost element of a structure.
--
-- >>> case ['a','b','c'] of (x :< _) -> x
-- 'a'
--
pattern (:<) :: forall s a. Cons s s a a => a -> s -> s
pattern (:<) a s <- (preview _Cons -> Just (a, s)) where
  (:<) a s = review _Cons (a, s)

infixr 5 :<
infixl 5 :>

-- | Pattern synonym for matching on the rightmost element of a structure.
--
-- >>> case ['a','b','c'] of (_ :> x) -> x
-- 'c'
--
pattern (:>) :: forall s a. Snoc s s a a => s -> a -> s
pattern (:>) s a <- (preview _Snoc -> Just (s, a)) where
  (:>) a s = review _Snoc (a, s)

------------------------------------------------------------------------------
-- Cons
------------------------------------------------------------------------------

-- | This class provides a way to attach or detach elements on the left
-- side of a structure in a flexible manner.
class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- |
  --
  -- @
  -- '_Cons' :: 'Prism' [a] [b] (a, [a]) (b, [b])
  -- '_Cons' :: 'Prism' ('Seq' a) ('Seq' b) (a, 'Seq' a) (b, 'Seq' b)
  -- '_Cons' :: 'Prism' (Vector a) (Vector b) (a, Vector a) (b, Vector b)
  -- '_Cons' :: 'Prism'' 'String' ('Char', 'String')
  -- '_Cons' :: 'Prism'' Text ('Char', Text)
  -- '_Cons' :: 'Prism'' ByteString ('Data.Word.Word8', ByteString)
  -- @
  _Cons :: Prism s t (a, s) (b, t)

instance Cons [a] [b] a b where
  _Cons = prism (uncurry (:)) $ \ aas -> case aas of
    (a:as) -> Right (a, as)
    []     -> Left  []
  {-# INLINE _Cons #-}

instance Cons (ZipList a) (ZipList b) a b where
  _Cons = coerceS . coerceT . coerceA . coerceB $ listCons
    where
      listCons :: Prism [a] [b] (a, [a]) (b, [b])
      listCons = _Cons

  {-# INLINE _Cons #-}

instance Cons (Seq a) (Seq b) a b where
  _Cons = prism (uncurry (Seq.<|)) $ \aas -> case viewl aas of
    a Seq.:< as -> Right (a, as)
    EmptyL  -> Left mempty
  {-# INLINE _Cons #-}

-- | 'cons' an element onto a container.
--
-- This is an infix alias for 'cons'.
--
-- >>> 1 <| []
-- [1]
--
-- >>> 'a' <| "bc"
-- "abc"
--
-- >>> 1 <| []
-- [1]
--
-- >>> 1 <| [2, 3]
-- [1,2,3]
(<|) :: Cons s s a a => a -> s -> s
(<|) = curry (review _Cons)
{-# INLINE (<|) #-}

-- | 'cons' an element onto a container.
--
-- >>> cons 'a' ""
-- "a"
--
-- >>> cons 'a' "bc"
-- "abc"
cons :: Cons s s a a => a -> s -> s
cons = curry (review _Cons)
{-# INLINE cons #-}

-- | Attempt to extract the left-most element from a container, and a version of
-- the container without that element.
--
-- >>> uncons []
-- Nothing
--
-- >>> uncons [1, 2, 3]
-- Just (1,[2,3])
uncons :: Cons s s a a => s -> Maybe (a, s)
uncons = preview _Cons
{-# INLINE uncons #-}

-- | An 'AffineTraversal' reading and writing to the 'head' of a /non-empty/
-- container.
--
-- >>> "abc" ^? _head
-- Just 'a'
--
-- >>> "abc" & _head .~ 'd'
-- "dbc"
--
-- >>> [1,2,3] & _head %~ (*10)
-- [10,2,3]
--
-- >>> [] & _head %~ absurd
-- []
--
-- >>> [1,2,3] ^? _head
-- Just 1
--
-- >>> [] ^? _head
-- Nothing
--
-- >>> [1,2] ^? _head
-- Just 1
--
-- >>> [] & _head .~ 1
-- []
--
-- >>> [0] & _head .~ 2
-- [2]
--
-- >>> [0,1] & _head .~ 2
-- [2,1]
_head :: Cons s s a a => AffineTraversal' s a
_head = _Cons % _1
{-# INLINE _head #-}

-- | An 'AffineTraversal' reading and writing to the 'tail' of a /non-empty/
-- container.
--
-- >>> "ab" & _tail .~ "cde"
-- "acde"
--
-- >>> [] & _tail .~ [1,2]
-- []
--
-- >>> [1,2,3,4,5] & _tail % traversed %~ (*10)
-- [1,20,30,40,50]
--
-- >>> [1,2] & _tail .~ [3,4,5]
-- [1,3,4,5]
--
-- >>> [] & _tail .~ [1,2]
-- []
--
-- >>> "abc" ^? _tail
-- Just "bc"
--
-- >>> "hello" ^? _tail
-- Just "ello"
--
-- >>> "" ^? _tail
-- Nothing
_tail :: Cons s s a a => AffineTraversal' s s
_tail = _Cons % _2
{-# INLINE _tail #-}

------------------------------------------------------------------------------
-- Snoc
------------------------------------------------------------------------------

-- | This class provides a way to attach or detach elements on the right side of
-- a structure in a flexible manner.
class Snoc s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc :: Prism s t (s, a) (t, b)

instance Snoc [a] [b] a b where
  _Snoc = prism (\(as,a) -> as Prelude.++ [a]) $ \aas -> if Prelude.null aas
    then Left []
    else Right (Prelude.init aas, Prelude.last aas)
  {-# INLINE _Snoc #-}

instance Snoc (ZipList a) (ZipList b) a b where
  _Snoc = withPrism listSnoc $ \listReview listPreview ->
    prism (coerce listReview) (coerce listPreview) where

    listSnoc :: Prism [a] [b] ([a], a) ([b], b)
    listSnoc = _Snoc

  {-# INLINE _Snoc #-}

instance Snoc (Seq a) (Seq b) a b where
  _Snoc = prism (uncurry (Seq.|>)) $ \aas -> case viewr aas of
    as Seq.:> a -> Right (as, a)
    EmptyR  -> Left mempty
  {-# INLINE _Snoc #-}

-- | An 'AffineTraversal' reading and replacing all but the a last element of a
-- /non-empty/ container.
--
-- >>> "abcd" ^? _init
-- Just "abc"
--
-- >>> "" ^? _init
-- Nothing
--
-- >>> "ab" & _init .~ "cde"
-- "cdeb"
--
-- >>> [] & _init .~ [1,2]
-- []
--
-- >>> [1,2,3,4] & _init % traversed %~ (*10)
-- [10,20,30,4]
--
-- >>> [1,2,3] ^? _init
-- Just [1,2]
--
-- >>> "hello" ^? _init
-- Just "hell"
--
-- >>> [] ^? _init
-- Nothing
_init :: Snoc s s a a => AffineTraversal' s s
_init = _Snoc % _1
{-# INLINE _init #-}

-- | An 'AffineTraversal' reading and writing to the last element of a
-- /non-empty/ container.
--
-- >>> "abc" ^? _last
-- Just 'c'
--
-- >>> "" ^? _last
-- Nothing
--
-- >>> [1,2,3] & _last %~ (+1)
-- [1,2,4]
--
-- >>> [1,2] ^? _last
-- Just 2
--
-- >>> [] & _last .~ 1
-- []
--
-- >>> [0] & _last .~ 2
-- [2]
--
-- >>> [0,1] & _last .~ 2
-- [0,2]
_last :: Snoc s s a a => AffineTraversal' s a
_last = _Snoc % _2
{-# INLINE _last #-}

-- | 'snoc' an element onto the end of a container.
--
-- This is an infix alias for 'snoc'.
--
-- >>> "" |> 'a'
-- "a"
--
-- >>> "bc" |> 'a'
-- "bca"
(|>) :: Snoc s s a a => s -> a -> s
(|>) = curry (review _Snoc)
{-# INLINE (|>) #-}

-- | 'snoc' an element onto the end of a container.
--
-- >>> snoc "hello" '!'
-- "hello!"
snoc  :: Snoc s s a a => s -> a -> s
snoc = curry (review _Snoc)
{-# INLINE snoc #-}

-- | Attempt to extract the right-most element from a container, and a version
-- of the container without that element.
--
-- >>> unsnoc "hello!"
-- Just ("hello",'!')
--
-- >>> unsnoc ""
-- Nothing
unsnoc :: Snoc s s a a => s -> Maybe (s, a)
unsnoc s = preview _Snoc s
{-# INLINE unsnoc #-}

-- $setup
-- >>> import Data.Void
-- >>> import Optics.Core
