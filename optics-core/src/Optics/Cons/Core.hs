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

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g

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
  -- '_Cons' :: 'Prism' ('Vector' a) ('Vector' b) (a, 'Vector' a) (b, 'Vector' b)
  -- '_Cons' :: 'Prism'' 'String' ('Char', 'String')
  -- '_Cons' :: 'Prism'' 'StrictT.Text' ('Char', 'StrictT.Text')
  -- '_Cons' :: 'Prism'' 'StrictB.ByteString' ('Word8', 'StrictB.ByteString')
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
-- >>> a <| []
-- [a]
--
-- >>> a <| [b, c]
-- [a,b,c]
--
-- >>> a <| Seq.fromList []
-- fromList [a]
--
-- >>> a <| Seq.fromList [b, c]
-- fromList [a,b,c]
(<|) :: Cons s s a a => a -> s -> s
(<|) = curry (review _Cons)
{-# INLINE (<|) #-}

-- | 'cons' an element onto a container.
--
-- >>> cons a []
-- [a]
--
-- >>> cons a [b, c]
-- [a,b,c]
--
-- >>> cons a (Seq.fromList [])
-- fromList [a]
--
-- >>> cons a (Seq.fromList [b, c])
-- fromList [a,b,c]
cons :: Cons s s a a => a -> s -> s
cons = curry (review _Cons)
{-# INLINE cons #-}

-- | Attempt to extract the left-most element from a container, and a version of
-- the container without that element.
--
-- >>> uncons []
-- Nothing
--
-- >>> uncons [a, b, c]
-- Just (a,[b,c])
uncons :: Cons s s a a => s -> Maybe (a, s)
uncons = preview _Cons
{-# INLINE uncons #-}

-- | An 'AffineTraversal' reading and writing to the 'head' of a /non-empty/
-- container.
--
-- >>> [a,b,c]^? _head
-- Just a
--
-- >>> [a,b,c] & _head .~ d
-- [d,b,c]
--
-- >>> [a,b,c] & _head %~ f
-- [f a,b,c]
--
-- >>> [] & _head %~ f
-- []
--
-- >>> [1,2,3]^?!_head
-- 1
--
-- >>> []^?_head
-- Nothing
--
-- >>> [1,2]^?_head
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
--
-- This isn't limited to lists.
--
-- For instance you can also 'Data.Traversable.traverse' the head of a 'Seq':
--
-- >>> Seq.fromList [a,b,c,d] & _head %~ f
-- fromList [f a,b,c,d]
--
-- >>> Seq.fromList [] ^? _head
-- Nothing
--
-- >>> Seq.fromList [a,b,c,d] ^? _head
-- Just a
--
-- @
-- '_head' :: 'AffineTraversal'' [a] a
-- '_head' :: 'AffineTraversal'' ('Seq' a) a
-- '_head' :: 'AffineTraversal'' ('Vector' a) a
-- @
_head :: Cons s s a a => AffineTraversal' s a
_head = _Cons % _1
{-# INLINE _head #-}

-- | An 'AffineTraversal' reading and writing to the 'tail' of a /non-empty/
-- container.
--
-- >>> [a,b] & _tail .~ [c,d,e]
-- [a,c,d,e]
--
-- >>> [] & _tail .~ [a,b]
-- []
--
-- >>> [a,b,c,d,e] & _tail.traverse %~ f
-- [a,f b,f c,f d,f e]
--
-- >>> [1,2] & _tail .~ [3,4,5]
-- [1,3,4,5]
--
-- >>> [] & _tail .~ [1,2]
-- []
--
-- >>> [a,b,c]^?_tail
-- Just [b,c]
--
-- >>> [1,2]^?!_tail
-- [2]
--
-- >>> "hello"^._tail
-- "ello"
--
-- >>> ""^._tail
-- ""
--
-- This isn't limited to lists. For instance you can also
-- 'Control.Traversable.traverse' the tail of a 'Seq'.
--
-- >>> Seq.fromList [a,b] & _tail .~ Seq.fromList [c,d,e]
-- fromList [a,c,d,e]
--
-- >>> Seq.fromList [a,b,c] ^? _tail
-- Just (fromList [b,c])
--
-- >>> Seq.fromList [] ^? _tail
-- Nothing
--
-- @
-- '_tail' :: 'AffineTraversal'' [a] [a]
-- '_tail' :: 'AffineTraversal'' ('Seq' a) ('Seq' a)
-- '_tail' :: 'AffineTraversal'' ('Vector' a) ('Vector' a)
-- @
_tail :: Cons s s a a => AffineTraversal' s s
_tail = _Cons % _2
{-# INLINE _tail #-}

------------------------------------------------------------------------------
-- Snoc
------------------------------------------------------------------------------

-- | This class provides a way to attach or detach elements on the right
-- side of a structure in a flexible manner.
class Snoc s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- |
  --
  -- @
  -- '_Snoc' :: 'Prism' [a] [b] ([a], a) ([b], b)
  -- '_Snoc' :: 'Prism' ('Seq' a) ('Seq' b) ('Seq' a, a) ('Seq' b, b)
  -- '_Snoc' :: 'Prism' ('Vector' a) ('Vector' b) ('Vector' a, a) ('Vector' b, b)
  -- '_Snoc' :: 'Prism'' 'String' ('String', 'Char')
  -- '_Snoc' :: 'Prism'' 'StrictT.Text' ('StrictT.Text', 'Char')
  -- '_Snoc' :: 'Prism'' 'StrictB.ByteString' ('StrictB.ByteString', 'Word8')
  -- @
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
-- >>> [a,b,c,d]^?_init
-- Just [a,b,c]
--
-- >>> []^?_init
-- Nothing
--
-- >>> [a,b] & _init .~ [c,d,e]
-- [c,d,e,b]
--
-- >>> [] & _init .~ [a,b]
-- []
--
-- >>> [a,b,c,d] & _init.traverse %~ f
-- [f a,f b,f c,d]
--
-- >>> [1,2,3]^?_init
-- Just [1,2]
--
-- >>> [1,2,3,4]^?!_init
-- [1,2,3]
--
-- >>> "hello"^._init
-- "hell"
--
-- >>> ""^._init
-- ""
--
-- @
-- '_init' :: 'AffineTraversal'' [a] [a]
-- '_init' :: 'AffineTraversal'' ('Seq' a) ('Seq' a)
-- '_init' :: 'AffineTraversal'' ('Vector' a) ('Vector' a)
-- @
_init :: Snoc s s a a => AffineTraversal' s s
_init = _Snoc % _1
{-# INLINE _init #-}

-- | An 'AffineTraversal' reading and writing to the last element of a
-- /non-empty/ container.
--
-- >>> [a,b,c]^?!_last
-- c
--
-- >>> []^?_last
-- Nothing
--
-- >>> [a,b,c] & _last %~ f
-- [a,b,f c]
--
-- >>> [1,2]^?_last
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
--
-- This 'AffineTraversal' is not limited to lists, however. We can also work
-- with other containers, such as a 'Vector'.
--
-- >>> Vector.fromList "abcde" ^? _last
-- Just 'e'
--
-- >>> Vector.empty ^? _last
-- Nothing
--
-- >>> (Vector.fromList "abcde" & _last .~ 'Q') == Vector.fromList "abcdQ"
-- True
--
-- @
-- '_last' :: 'AffineTraversal'' [a] a
-- '_last' :: 'AffineTraversal'' ('Seq' a) a
-- '_last' :: 'AffineTraversal'' ('Vector' a) a
-- @
_last :: Snoc s s a a => AffineTraversal' s a
_last = _Snoc % _2
{-# INLINE _last #-}

-- | 'snoc' an element onto the end of a container.
--
-- This is an infix alias for 'snoc'.
--
-- >>> Seq.fromList [] |> a
-- fromList [a]
--
-- >>> Seq.fromList [b, c] |> a
-- fromList [b,c,a]
--
-- >>> LazyT.pack "hello" |> '!'
-- "hello!"
(|>) :: Snoc s s a a => s -> a -> s
(|>) = curry (review _Snoc)
{-# INLINE (|>) #-}

-- | 'snoc' an element onto the end of a container.
--
-- >>> snoc (Seq.fromList []) a
-- fromList [a]
--
-- >>> snoc (Seq.fromList [b, c]) a
-- fromList [b,c,a]
--
-- >>> snoc (LazyT.pack "hello") '!'
-- "hello!"
snoc  :: Snoc s s a a => s -> a -> s
snoc = curry (review _Snoc)
{-# INLINE snoc #-}

-- | Attempt to extract the right-most element from a container, and a version
-- of the container without that element.
--
-- >>> unsnoc (LazyT.pack "hello!")
-- Just ("hello",'!')
--
-- >>> unsnoc (LazyT.pack "")
-- Nothing
--
-- >>> unsnoc (Seq.fromList [b,c,a])
-- Just (fromList [b,c],a)
--
-- >>> unsnoc (Seq.fromList [])
-- Nothing
unsnoc :: Snoc s s a a => s -> Maybe (s, a)
unsnoc s = preview _Snoc s
{-# INLINE unsnoc #-}
