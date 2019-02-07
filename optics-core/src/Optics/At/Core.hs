{-# LANGUAGE CPP #-}
module Optics.At.Core
  (
  -- * At
    At(..)
  , sans
  -- * Ixed
  , Index
  , IxValue
  , Ixed(ix)
  , ixAt
  -- * Contains
  , Contains(..)
  ) where

import Data.Array.IArray as Array
import Data.Array.Unboxed
import Data.Complex
import Data.Functor.Identity
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tree

import Data.Maybe.Optics
import Optics.AffineTraversal
import Optics.Lens
import Optics.Setter
import Optics.Operators ((<&>))

type family Index (s :: *) :: *
type instance Index (e -> a) = e
type instance Index IntSet = Int
type instance Index (Set a) = a
type instance Index [a] = Int
type instance Index (NonEmpty a) = Int
type instance Index (Seq a) = Int
type instance Index (a,b) = Int
type instance Index (a,b,c) = Int
type instance Index (a,b,c,d) = Int
type instance Index (a,b,c,d,e) = Int
type instance Index (a,b,c,d,e,f) = Int
type instance Index (a,b,c,d,e,f,g) = Int
type instance Index (a,b,c,d,e,f,g,h) = Int
type instance Index (a,b,c,d,e,f,g,h,i) = Int
type instance Index (IntMap a) = Int
type instance Index (Map k a) = k
type instance Index (Array.Array i e) = i
type instance Index (UArray i e) = i
type instance Index (Complex a) = Int
type instance Index (Identity a) = ()
type instance Index (Maybe a) = ()
type instance Index (Tree a) = [Int]

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> let f  :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g  :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let f' :: Int -> Expr -> Expr; f' = Debug.SimpleReflect.Vars.f'
-- >>> let h  :: Int -> Expr; h = Debug.SimpleReflect.Vars.h

-- | This class provides a simple 'Lens' that lets you view (and modify)
-- information about whether or not a container contains a given 'Index'.
class Contains m where
  -- |
  -- >>> IntSet.fromList [1,2,3,4] ^. contains 3
  -- True
  --
  -- >>> IntSet.fromList [1,2,3,4] ^. contains 5
  -- False
  --
  -- >>> IntSet.fromList [1,2,3,4] & contains 3 .~ False
  -- fromList [1,2,4]
  contains :: Index m -> Lens' m Bool

instance Contains IntSet where
  contains k = lensVL $ \f s -> f (IntSet.member k s) <&> \b ->
    if b then IntSet.insert k s else IntSet.delete k s
  {-# INLINE contains #-}

instance Ord a => Contains (Set a) where
  contains k = lensVL $ \f s -> f (Set.member k s) <&> \b ->
    if b then Set.insert k s else Set.delete k s
  {-# INLINE contains #-}

-- | This provides a common notion of a value at an index that is shared by both
-- 'Ixed' and 'At'.
type family IxValue (m :: *) :: *

-- | Provides a simple 'AffineTraversal' lets you traverse the value at a given
-- key in a 'Map' or element at an ordinal position in a list or 'Seq'.
class Ixed m where
  -- | /NB:/ Setting the value of this 'AffineTraversal' will only set the value
  -- in 'at' if it is already present.
  --
  -- If you want to be able to insert /missing/ values, you want 'at'.
  --
  -- >>> Seq.fromList [a,b,c,d] & ix 2 %~ f
  -- fromList [a,b,f c,d]
  --
  -- >>> Seq.fromList [a,b,c,d] & ix 2 .~ e
  -- fromList [a,b,e,d]
  --
  -- >>> Seq.fromList [a,b,c,d] ^? ix 2
  -- Just c
  --
  -- >>> Seq.fromList [] ^? ix 2
  -- Nothing
  ix :: Index m -> AffineTraversal' m (IxValue m)
  default ix :: At m => Index m -> AffineTraversal' m (IxValue m)
  ix = ixAt
  {-# INLINE ix #-}

-- | A definition of 'ix' for types with an 'At' instance. This is the default
-- if you don't specify a definition for 'ix'.
ixAt :: At m => Index m -> AffineTraversal' m (IxValue m)
ixAt = \i -> at i % _Just
{-# INLINE ixAt #-}

type instance IxValue (e -> a) = a
instance Eq e => Ixed (e -> a) where
  ix e = atraversalVL $ \_ p f -> p (f e) <&> \a e' -> if e == e' then a else f e'
  {-# INLINE ix #-}

type instance IxValue (Maybe a) = a
instance Ixed (Maybe a) where
  ix () = toAffineTraversal _Just
  {-# INLINE ix #-}

type instance IxValue [a] = a
instance Ixed [a] where
  ix k = atraversalVL (ixListVL k)
  {-# INLINE ix #-}

type instance IxValue (NonEmpty a) = a
instance Ixed (NonEmpty a) where
  ix k = atraversalVL $ \point f xs0 ->
    if k < 0
    then point xs0
    else let go (a:|as) 0 = f a <&> (:|as)
             go (a:|as) i = (a:|) <$> ixListVL (i - 1) point f as
         in go xs0 k
  {-# INLINE ix #-}

type instance IxValue (Identity a) = a
instance Ixed (Identity a) where
  ix () = atraversalVL $ \_ f (Identity a) -> Identity <$> f a
  {-# INLINE ix #-}

type instance IxValue (Tree a) = a
instance Ixed (Tree a) where
  ix xs0 = atraversalVL $ \point f ->
    let go [] (Node a as) = f a <&> \a' -> Node a' as
        go (i:is) t@(Node a as)
          | i < 0     = point t
          | otherwise = Node a <$> ixListVL i point (go is) as
    in go xs0
  {-# INLINE ix #-}

type instance IxValue (Seq a) = a
instance Ixed (Seq a) where
  ix i = atraversalVL $ \point f m ->
    if 0 <= i && i < Seq.length m
    then f (Seq.index m i) <&> \a -> Seq.update i a m
    else point m
  {-# INLINE ix #-}

type instance IxValue (IntMap a) = a
-- Default implementation uses IntMap.alterF
instance Ixed (IntMap a)

type instance IxValue (Map k a) = a
-- Default implementation uses Map.alterF
instance Ord k => Ixed (Map k a)

type instance IxValue (Set k) = ()
instance Ord k => Ixed (Set k) where
  ix k = atraversalVL $ \point f m ->
    if Set.member k m
    then f () <&> \() -> Set.insert k m
    else point m
  {-# INLINE ix #-}

type instance IxValue IntSet = ()
instance Ixed IntSet where
  ix k = atraversalVL $ \point f m ->
    if IntSet.member k m
    then f () <&> \() -> IntSet.insert k m
    else point m
  {-# INLINE ix #-}

type instance IxValue (Array.Array i e) = e
-- |
-- @
-- arr '!' i ≡ arr '^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i '.~' e '$' arr
-- @
instance Ix i => Ixed (Array.Array i e) where
  ix i = atraversalVL $ \point f arr ->
    if inRange (bounds arr) i
    then f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else point arr
  {-# INLINE ix #-}

type instance IxValue (UArray i e) = e
-- |
-- @
-- arr '!' i ≡ arr '^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i '.~' e '$' arr
-- @
instance (IArray UArray e, Ix i) => Ixed (UArray i e) where
  ix i = atraversalVL $ \point f arr ->
    if inRange (bounds arr) i
    then f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else point arr
  {-# INLINE ix #-}

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a) a@
type instance IxValue (a0, a2) = a0
instance (a0 ~ a1) => Ixed (a0, a1) where
  ix i = atraversalVL $ \point f ~s@(a0, a1) ->
    case i of
      0 -> (,a1) <$> f a0
      1 -> (a0,) <$> f a1
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a) a@
type instance IxValue (a0, a1, a2) = a0
instance (a0 ~ a1, a0 ~ a2) => Ixed (a0, a1, a2) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2) ->
    case i of
      0 -> (,a1,a2) <$> f a0
      1 -> (a0,,a2) <$> f a1
      2 -> (a0,a1,) <$> f a2
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a, a) a@
type instance IxValue (a0, a1, a2, a3) = a0
instance (a0 ~ a1, a0 ~ a2, a0 ~ a3) => Ixed (a0, a1, a2, a3) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2, a3) ->
    case i of
      0 -> (,a1,a2,a3) <$> f a0
      1 -> (a0,,a2,a3) <$> f a1
      2 -> (a0,a1,,a3) <$> f a2
      3 -> (a0,a1,a2,) <$> f a3
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a, a, a) a@
type instance IxValue (a0, a1, a2, a3, a4) = a0
instance (a0 ~ a1, a0 ~ a2, a0 ~ a3, a0 ~ a4) => Ixed (a0, a1, a2, a3, a4) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2, a3, a4) ->
    case i of
      0 -> (,a1,a2,a3,a4) <$> f a0
      1 -> (a0,,a2,a3,a4) <$> f a1
      2 -> (a0,a1,,a3,a4) <$> f a2
      3 -> (a0,a1,a2,,a4) <$> f a3
      4 -> (a0,a1,a2,a3,) <$> f a4
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a, a, a, a) a@
type instance IxValue (a0, a1, a2, a3, a4, a5) = a0
instance
  (a0 ~ a1, a0 ~ a2, a0 ~ a3, a0 ~ a4, a0 ~ a5
  ) => Ixed (a0, a1, a2, a3, a4, a5) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2, a3, a4, a5) ->
    case i of
      0 -> (,a1,a2,a3,a4,a5) <$> f a0
      1 -> (a0,,a2,a3,a4,a5) <$> f a1
      2 -> (a0,a1,,a3,a4,a5) <$> f a2
      3 -> (a0,a1,a2,,a4,a5) <$> f a3
      4 -> (a0,a1,a2,a3,,a5) <$> f a4
      5 -> (a0,a1,a2,a3,a4,) <$> f a5
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a, a, a, a, a) a@
type instance IxValue (a0, a1, a2, a3, a4, a5, a6) = a0
instance
  (a0 ~ a1, a0 ~ a2, a0 ~ a3, a0 ~ a4, a0 ~ a5, a0 ~ a6
  ) => Ixed (a0, a1, a2, a3, a4, a5, a6) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2, a3, a4, a5, a6) ->
    case i of
      0 -> (,a1,a2,a3,a4,a5,a6) <$> f a0
      1 -> (a0,,a2,a3,a4,a5,a6) <$> f a1
      2 -> (a0,a1,,a3,a4,a5,a6) <$> f a2
      3 -> (a0,a1,a2,,a4,a5,a6) <$> f a3
      4 -> (a0,a1,a2,a3,,a5,a6) <$> f a4
      5 -> (a0,a1,a2,a3,a4,,a6) <$> f a5
      6 -> (a0,a1,a2,a3,a4,a5,) <$> f a6
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a, a, a, a, a, a) a@
type instance IxValue (a0, a1, a2, a3, a4, a5, a6, a7) = a0
instance
  (a0 ~ a1, a0 ~ a2, a0 ~ a3, a0 ~ a4, a0 ~ a5, a0 ~ a6, a0 ~ a7
  ) => Ixed (a0, a1, a2, a3, a4, a5, a6, a7) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2, a3, a4, a5, a6, a7) ->
    case i of
      0 -> (,a1,a2,a3,a4,a5,a6,a7) <$> f a0
      1 -> (a0,,a2,a3,a4,a5,a6,a7) <$> f a1
      2 -> (a0,a1,,a3,a4,a5,a6,a7) <$> f a2
      3 -> (a0,a1,a2,,a4,a5,a6,a7) <$> f a3
      4 -> (a0,a1,a2,a3,,a5,a6,a7) <$> f a4
      5 -> (a0,a1,a2,a3,a4,,a6,a7) <$> f a5
      6 -> (a0,a1,a2,a3,a4,a5,,a7) <$> f a6
      7 -> (a0,a1,a2,a3,a4,a5,a6,) <$> f a7
      _ -> point s

-- | @'ix' :: 'Int' -> 'AffineTraversal'' (a, a, a, a, a, a, a, a, a) a@
type instance IxValue (a0, a1, a2, a3, a4, a5, a6, a7, a8) = a0
instance
  (a0 ~ a1, a0 ~ a2, a0 ~ a3, a0 ~ a4, a0 ~ a5, a0 ~ a6, a0 ~ a7, a0 ~ a8
  ) => Ixed (a0, a1, a2, a3, a4, a5, a6, a7, a8) where
  ix i = atraversalVL $ \point f ~s@(a0, a1, a2, a3, a4, a5, a6, a7, a8) ->
    case i of
      0 -> (,a1,a2,a3,a4,a5,a6,a7,a8) <$> f a0
      1 -> (a0,,a2,a3,a4,a5,a6,a7,a8) <$> f a1
      2 -> (a0,a1,,a3,a4,a5,a6,a7,a8) <$> f a2
      3 -> (a0,a1,a2,,a4,a5,a6,a7,a8) <$> f a3
      4 -> (a0,a1,a2,a3,,a5,a6,a7,a8) <$> f a4
      5 -> (a0,a1,a2,a3,a4,,a6,a7,a8) <$> f a5
      6 -> (a0,a1,a2,a3,a4,a5,,a7,a8) <$> f a6
      7 -> (a0,a1,a2,a3,a4,a5,a6,,a8) <$> f a7
      8 -> (a0,a1,a2,a3,a4,a5,a6,a7,) <$> f a8
      _ -> point s

-- | 'At' provides a 'Lens' that can be used to read, write or delete the value
-- associated with a key in a 'Map'-like container on an ad hoc basis.
--
-- An instance of 'At' should satisfy:
--
-- @
-- 'ix' k ≡ 'at' k '%' '_Just'
-- @
class Ixed m => At m where
  -- |
  -- >>> Map.fromList [(1,"world")] ^.at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not
  -- 'Array'-like ones, where you cannot satisfy the 'Lens' laws.
  at :: Index m -> Lens' m (Maybe (IxValue m))

-- | Delete the value associated with a key in a 'Map'-like container
--
-- @
-- 'sans' k = 'at' k .~ Nothing
-- @
sans :: At m => Index m -> m -> m
sans k = set (at k) Nothing
{-# INLINE sans #-}

instance At (Maybe a) where
  at () = lensVL id
  {-# INLINE at #-}

instance At (IntMap a) where
#if MIN_VERSION_containers(0,5,8)
  at k = lensVL $ \f -> IntMap.alterF f k
#else
  at k = lensVL $ \f m ->
    let mv = IntMap.lookup k m
    in f mv <&> \r -> case r of
      Nothing -> maybe m (const (IntMap.delete k m)) mv
      Just v' -> IntMap.insert k v' m
#endif
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
#if MIN_VERSION_containers(0,5,8)
  at k = lensVL $ \f -> Map.alterF f k
#else
  at k = lensVL $ \f m ->
    let mv = Map.lookup k m
    in f mv <&> \r -> case r of
      Nothing -> maybe m (const (Map.delete k m)) mv
      Just v' -> Map.insert k v' m
#endif
  {-# INLINE at #-}

instance At IntSet where
  at k = lensVL $ \f m ->
    let mv = if IntSet.member k m
             then Just ()
             else Nothing
    in f mv <&> \r -> case r of
      Nothing -> maybe m (const (IntSet.delete k m)) mv
      Just () -> IntSet.insert k m
  {-# INLINE at #-}

instance Ord k => At (Set k) where
  at k = lensVL $ \f m ->
    let mv = if Set.member k m
             then Just ()
             else Nothing
    in f mv <&> \r -> case r of
      Nothing -> maybe m (const (Set.delete k m)) mv
      Just () -> Set.insert k m
  {-# INLINE at #-}

----------------------------------------
-- Internal

ixListVL :: Int -> AffineTraversalVL' [a] a
ixListVL k point f xs0 =
  if k < 0
  then point xs0
  else let go [] _ = point []
           go (a:as) 0 = f a <&> (:as)
           go (a:as) i = (a:) <$> (go as $! i - 1)
       in go xs0 k
{-# INLINE ixListVL #-}
