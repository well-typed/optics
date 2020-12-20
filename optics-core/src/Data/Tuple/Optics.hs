-- |
-- Module: Data.Tuple.Optics
-- Description: 'Lens'es for tuple types.
--
-- This module defines 'Lens'es for the fields of tuple types.  These are
-- overloaded using the 'Field1' to 'Field9' typeclasses, so that '_1' can be
-- used as a 'Lens' for the first field of a tuple with any number of fields (up
-- to the maximum supported tuple size, which is currently 9).  For example:
--
-- >>> view _1 ('a','b','c')
-- 'a'
--
-- >>> set _3 True ('a','b','c')
-- ('a','b',True)
--
-- If a datatype has a 'Generic' instance, the corresponding @FieldN@ instances
-- can be defined using their default methods:
--
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics (Generic)
-- >>> data T a b = MkT a Int b deriving (Generic, Show)
-- >>> instance Field1 (T a c) (T b c) a b
-- >>> instance Field2 (T a b) (T a b) Int Int
-- >>> instance Field3 (T c a) (T c b) a b
--
-- >>> set _3 'x' (MkT False 1 ())
-- MkT False 1 'x'
--
-- For a generalization of this pattern see 'GPosition'.
--
module Data.Tuple.Optics
  (
  -- * Tuples
    Field1(..)
  , Field2(..)
  , Field3(..)
  , Field4(..)
  , Field5(..)
  , Field6(..)
  , Field7(..)
  , Field8(..)
  , Field9(..)
  -- * Strict variations
  , _1', _2', _3', _4', _5', _6', _7', _8', _9'
  ) where

import Data.Functor.Identity
import Data.Functor.Product
import GHC.Generics ((:*:)(..))

import Optics.Generic
import Optics.Lens
import Optics.Optic

-- | Provides access to 1st field of a tuple.
class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 1st field of a tuple (and possibly change its type).
  --
  -- >>> (1,2) ^. _1
  -- 1
  --
  -- >>> (1,2) & _1 .~ "hello"
  -- ("hello",2)
  --
  -- >>> traverseOf _1 putStrLn ("hello","world")
  -- hello
  -- ((),"world")
  --
  -- This can also be used on larger tuples as well:
  --
  -- >>> (1,2,3,4,5) & _1 %~ (+41)
  -- (42,2,3,4,5)
  _1 :: Lens s t a b
  default _1 :: GPosition 1 s t a b => Lens s t a b
  _1 = gposition @1
  {-# INLINE[1] _1 #-}

instance Field1 (Identity a) (Identity b) a b where
  _1 = lensVL $ \f (Identity a) -> Identity <$> f a
  {-# INLINE[1] _1 #-}

instance Field1 (Product f g a) (Product f' g a) (f a) (f' a) where
  _1 = lensVL $ \f ~(Pair a b) -> flip Pair b <$> f a
  {-# INLINE[1] _1 #-}

instance Field1 ((f :*: g) p) ((f' :*: g) p) (f p) (f' p) where
  _1 = lensVL $ \f ~(l :*: r) -> (:*: r) <$> f l
  {-# INLINE[1] _1 #-}

instance Field1 (a,b) (a',b) a a' where
  _1 = lensVL $ \k ~(a,b) -> k a <&> \a' -> (a',b)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c) (a',b,c) a a' where
  _1 = lensVL $ \k ~(a,b,c) -> k a <&> \a' -> (a',b,c)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c,d) (a',b,c,d) a a' where
  _1 = lensVL $ \k ~(a,b,c,d) -> k a <&> \a' -> (a',b,c,d)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c,d,e) (a',b,c,d,e) a a' where
  _1 = lensVL $ \k ~(a,b,c,d,e) -> k a <&> \a' -> (a',b,c,d,e)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c,d,e,f) (a',b,c,d,e,f) a a' where
  _1 = lensVL $ \k ~(a,b,c,d,e,f) -> k a <&> \a' -> (a',b,c,d,e,f)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c,d,e,f,g) (a',b,c,d,e,f,g) a a' where
  _1 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k a <&> \a' -> (a',b,c,d,e,f,g)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c,d,e,f,g,h) (a',b,c,d,e,f,g,h) a a' where
  _1 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k a <&> \a' -> (a',b,c,d,e,f,g,h)
  {-# INLINE[1] _1 #-}

instance Field1 (a,b,c,d,e,f,g,h,i) (a',b,c,d,e,f,g,h,i) a a' where
  _1 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k a <&> \a' -> (a',b,c,d,e,f,g,h,i)
  {-# INLINE[1] _1 #-}

-- | Provides access to the 2nd field of a tuple.
class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 2nd field of a tuple.
  --
  -- >>> _2 .~ "hello" $ (1,(),3,4)
  -- (1,"hello",3,4)
  --
  -- >>> (1,2,3,4) & _2 %~ (*3)
  -- (1,6,3,4)
  --
  -- >>> traverseOf _2 print (1,2)
  -- 2
  -- (1,())
  _2 :: Lens s t a b
  default _2 :: GPosition 2 s t a b => Lens s t a b
  _2 = gposition @2
  {-# INLINE[1] _2 #-}

instance Field2 (Product f g a) (Product f g' a) (g a) (g' a) where
  _2 = lensVL $ \f ~(Pair a b) -> Pair a <$> f b
  {-# INLINE[1] _2 #-}

instance Field2 ((f :*: g) p) ((f :*: g') p) (g p) (g' p) where
  _2 = lensVL $ \f ~(l :*: r) -> (l :*:) <$> f r
  {-# INLINE[1] _2 #-}

instance Field2 (a,b) (a,b') b b' where
  _2 = lensVL $ \k ~(a,b) -> k b <&> \b' -> (a,b')
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c) (a,b',c) b b' where
  _2 = lensVL $ \k ~(a,b,c) -> k b <&> \b' -> (a,b',c)
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c,d) (a,b',c,d) b b' where
  _2 = lensVL $ \k ~(a,b,c,d) -> k b <&> \b' -> (a,b',c,d)
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c,d,e) (a,b',c,d,e) b b' where
  _2 = lensVL $ \k ~(a,b,c,d,e) -> k b <&> \b' -> (a,b',c,d,e)
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c,d,e,f) (a,b',c,d,e,f) b b' where
  _2 = lensVL $ \k ~(a,b,c,d,e,f) -> k b <&> \b' -> (a,b',c,d,e,f)
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c,d,e,f,g) (a,b',c,d,e,f,g) b b' where
  _2 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k b <&> \b' -> (a,b',c,d,e,f,g)
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c,d,e,f,g,h) (a,b',c,d,e,f,g,h) b b' where
  _2 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k b <&> \b' -> (a,b',c,d,e,f,g,h)
  {-# INLINE[1] _2 #-}

instance Field2 (a,b,c,d,e,f,g,h,i) (a,b',c,d,e,f,g,h,i) b b' where
  _2 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k b <&> \b' -> (a,b',c,d,e,f,g,h,i)
  {-# INLINE[1] _2 #-}

-- | Provides access to the 3rd field of a tuple.
class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 3rd field of a tuple.
  _3 :: Lens s t a b
  default _3 :: GPosition 3 s t a b => Lens s t a b
  _3 = gposition @3
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c) (a,b,c') c c' where
  _3 = lensVL $ \k ~(a,b,c) -> k c <&> \c' -> (a,b,c')
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c,d) (a,b,c',d) c c' where
  _3 = lensVL $ \k ~(a,b,c,d) -> k c <&> \c' -> (a,b,c',d)
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c,d,e) (a,b,c',d,e) c c' where
  _3 = lensVL $ \k ~(a,b,c,d,e) -> k c <&> \c' -> (a,b,c',d,e)
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c,d,e,f) (a,b,c',d,e,f) c c' where
  _3 = lensVL $ \k ~(a,b,c,d,e,f) -> k c <&> \c' -> (a,b,c',d,e,f)
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c,d,e,f,g) (a,b,c',d,e,f,g) c c' where
  _3 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k c <&> \c' -> (a,b,c',d,e,f,g)
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c,d,e,f,g,h) (a,b,c',d,e,f,g,h) c c' where
  _3 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k c <&> \c' -> (a,b,c',d,e,f,g,h)
  {-# INLINE[1] _3 #-}

instance Field3 (a,b,c,d,e,f,g,h,i) (a,b,c',d,e,f,g,h,i) c c' where
  _3 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k c <&> \c' -> (a,b,c',d,e,f,g,h,i)
  {-# INLINE[1] _3 #-}

-- | Provide access to the 4th field of a tuple.
class Field4 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 4th field of a tuple.
  _4 :: Lens s t a b
  default _4 :: GPosition 4 s t a b => Lens s t a b
  _4 = gposition @4
  {-# INLINE[1] _4 #-}

instance Field4 (a,b,c,d) (a,b,c,d') d d' where
  _4 = lensVL $ \k ~(a,b,c,d) -> k d <&> \d' -> (a,b,c,d')
  {-# INLINE[1] _4 #-}

instance Field4 (a,b,c,d,e) (a,b,c,d',e) d d' where
  _4 = lensVL $ \k ~(a,b,c,d,e) -> k d <&> \d' -> (a,b,c,d',e)
  {-# INLINE[1] _4 #-}

instance Field4 (a,b,c,d,e,f) (a,b,c,d',e,f) d d' where
  _4 = lensVL $ \k ~(a,b,c,d,e,f) -> k d <&> \d' -> (a,b,c,d',e,f)
  {-# INLINE[1] _4 #-}

instance Field4 (a,b,c,d,e,f,g) (a,b,c,d',e,f,g) d d' where
  _4 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k d <&> \d' -> (a,b,c,d',e,f,g)
  {-# INLINE[1] _4 #-}

instance Field4 (a,b,c,d,e,f,g,h) (a,b,c,d',e,f,g,h) d d' where
  _4 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k d <&> \d' -> (a,b,c,d',e,f,g,h)
  {-# INLINE[1] _4 #-}

instance Field4 (a,b,c,d,e,f,g,h,i) (a,b,c,d',e,f,g,h,i) d d' where
  _4 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k d <&> \d' -> (a,b,c,d',e,f,g,h,i)
  {-# INLINE[1] _4 #-}

-- | Provides access to the 5th field of a tuple.
class Field5 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 5th field of a tuple.
  _5 :: Lens s t a b
  default _5 :: GPosition 5 s t a b => Lens s t a b
  _5 = gposition @5
  {-# INLINE[1] _5 #-}

instance Field5 (a,b,c,d,e) (a,b,c,d,e') e e' where
  _5 = lensVL $ \k ~(a,b,c,d,e) -> k e <&> \e' -> (a,b,c,d,e')
  {-# INLINE[1] _5 #-}

instance Field5 (a,b,c,d,e,f) (a,b,c,d,e',f) e e' where
  _5 = lensVL $ \k ~(a,b,c,d,e,f) -> k e <&> \e' -> (a,b,c,d,e',f)
  {-# INLINE[1] _5 #-}

instance Field5 (a,b,c,d,e,f,g) (a,b,c,d,e',f,g) e e' where
  _5 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k e <&> \e' -> (a,b,c,d,e',f,g)
  {-# INLINE[1] _5 #-}

instance Field5 (a,b,c,d,e,f,g,h) (a,b,c,d,e',f,g,h) e e' where
  _5 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k e <&> \e' -> (a,b,c,d,e',f,g,h)
  {-# INLINE[1] _5 #-}

instance Field5 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e',f,g,h,i) e e' where
  _5 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k e <&> \e' -> (a,b,c,d,e',f,g,h,i)
  {-# INLINE[1] _5 #-}

-- | Provides access to the 6th element of a tuple.
class Field6 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 6th field of a tuple.
  _6 :: Lens s t a b
  default _6 :: GPosition 6 s t a b => Lens s t a b
  _6 = gposition @6
  {-# INLINE[1] _6 #-}

instance Field6 (a,b,c,d,e,f) (a,b,c,d,e,f') f f' where
  _6 = lensVL $ \k ~(a,b,c,d,e,f) -> k f <&> \f' -> (a,b,c,d,e,f')
  {-# INLINE[1] _6 #-}

instance Field6 (a,b,c,d,e,f,g) (a,b,c,d,e,f',g) f f' where
  _6 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k f <&> \f' -> (a,b,c,d,e,f',g)
  {-# INLINE[1] _6 #-}

instance Field6 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f',g,h) f f' where
  _6 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k f <&> \f' -> (a,b,c,d,e,f',g,h)
  {-# INLINE[1] _6 #-}

instance Field6 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f',g,h,i) f f' where
  _6 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k f <&> \f' -> (a,b,c,d,e,f',g,h,i)
  {-# INLINE[1] _6 #-}

-- | Provide access to the 7th field of a tuple.
class Field7 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 7th field of a tuple.
  _7 :: Lens s t a b
  default _7 :: GPosition 7 s t a b => Lens s t a b
  _7 = gposition @7
  {-# INLINE[1] _7 #-}

instance Field7 (a,b,c,d,e,f,g) (a,b,c,d,e,f,g') g g' where
  _7 = lensVL $ \k ~(a,b,c,d,e,f,g) -> k g <&> \g' -> (a,b,c,d,e,f,g')
  {-# INLINE[1] _7 #-}

instance Field7 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g',h) g g' where
  _7 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k g <&> \g' -> (a,b,c,d,e,f,g',h)
  {-# INLINE[1] _7 #-}

instance Field7 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g',h,i) g g' where
  _7 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k g <&> \g' -> (a,b,c,d,e,f,g',h,i)
  {-# INLINE[1] _7 #-}

-- | Provide access to the 8th field of a tuple.
class Field8 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 8th field of a tuple.
  _8 :: Lens s t a b
  default _8 :: GPosition 8 s t a b => Lens s t a b
  _8 = gposition @8
  {-# INLINE[1] _8 #-}

instance Field8 (a,b,c,d,e,f,g,h) (a,b,c,d,e,f,g,h') h h' where
  _8 = lensVL $ \k ~(a,b,c,d,e,f,g,h) -> k h <&> \h' -> (a,b,c,d,e,f,g,h')
  {-# INLINE[1] _8 #-}

instance Field8 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h',i) h h' where
  _8 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k h <&> \h' -> (a,b,c,d,e,f,g,h',i)
  {-# INLINE[1] _8 #-}

-- | Provides access to the 9th field of a tuple.
class Field9 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  -- | Access the 9th field of a tuple.
  _9 :: Lens s t a b
  default _9 :: GPosition 9 s t a b => Lens s t a b
  _9 = gposition @9
  {-# INLINE[1] _9 #-}

instance Field9 (a,b,c,d,e,f,g,h,i) (a,b,c,d,e,f,g,h,i') i i' where
  _9 = lensVL $ \k ~(a,b,c,d,e,f,g,h,i) -> k i <&> \i' -> (a,b,c,d,e,f,g,h,i')
  {-# INLINE[1] _9 #-}

-- Strict versions of the _1 .. _9 operations

-- | Strict version of '_1'
_1' :: Field1 s t a b => Lens s t a b
_1' = equality' % _1
{-# INLINE _1' #-}

-- | Strict version of '_2'
_2' :: Field2 s t a b => Lens s t a b
_2' = equality' % _2
{-# INLINE _2' #-}

-- | Strict version of '_3'
_3' :: Field3 s t a b => Lens s t a b
_3' = equality' % _3
{-# INLINE _3' #-}

-- | Strict version of '_4'
_4' :: Field4 s t a b => Lens s t a b
_4' = equality' % _4
{-# INLINE _4' #-}

-- | Strict version of '_5'
_5' :: Field5 s t a b => Lens s t a b
_5' = equality' % _5
{-# INLINE _5' #-}

-- | Strict version of '_6'
_6' :: Field6 s t a b => Lens s t a b
_6' = equality' % _6
{-# INLINE _6' #-}

-- | Strict version of '_7'
_7' :: Field7 s t a b => Lens s t a b
_7' = equality' % _7
{-# INLINE _7' #-}

-- | Strict version of '_8'
_8' :: Field8 s t a b => Lens s t a b
_8' = equality' % _8
{-# INLINE _8' #-}

-- | Strict version of '_9'
_9' :: Field9 s t a b => Lens s t a b
_9' = equality' % _9
{-# INLINE _9' #-}

-- $setup
-- >>> import Optics.Core
