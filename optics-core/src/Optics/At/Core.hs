{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeInType #-}
-- |
-- Module: Optics.At.Core
-- Description: Optics for 'Map' and 'Set'-like containers.
--
-- This module provides optics for 'Map' and 'Set'-like containers, including an
-- 'AffineTraversal' to traverse a key in a map or an element of a sequence:
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
-- The @Optics.At@ module from @optics-extra@ provides additional instances of
-- the classes defined here.
--
module Optics.At.Core
  (
    -- * Type families
    Index
  , IxValue

    -- * Ixed
  , Ixed(..)
  , ixAt

    -- * At
  , At(..)
  , at'
  , sans

  -- * Contains
  , Contains(..)
  ) where

import qualified Data.Array.IArray as Array
import Data.Array.Unboxed (UArray)
import Data.Complex (Complex (..))
import Data.Ix (Ix (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Tree (Tree (..))

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe.Optics
import Optics.AffineTraversal
import Optics.Iso
import Optics.Lens
import Optics.Optic
import Optics.Setter

-- | Type family that takes a key-value container type and returns the type of
-- keys (indices) into the container, for example @'Index' ('Map' k a) ~ k@.
-- This is shared by 'Ixed', 'At' and 'Contains'.
type family Index (s :: Type) :: Type
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

-- | This class provides a simple 'Lens' that lets you view (and modify)
-- information about whether or not a container contains a given 'Index'.
-- Instances are provided for 'Set'-like containers only.
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

-- | Type family that takes a key-value container type and returns the type of
-- values stored in the container, for example @'IxValue' ('Map' k a) ~ a@. This
-- is shared by both 'Ixed' and 'At'.
type family IxValue (m :: Type) :: Type


-- | Provides a simple 'AffineTraversal' lets you traverse the value at a given
-- key in a 'Map' or element at an ordinal position in a list or 'Seq'.
class Ixed m where
  -- | Type family that takes a key-value container type and returns the kind
  -- of optic to index into it. For most containers, it's 'An_AffineTraversal',
  -- @Representable@ (Naperian) containers it is 'A_Lens', and multi-maps would
  -- have 'A_Traversal'.
  type IxKind (m :: Type) :: OpticKind
  type IxKind m = An_AffineTraversal

  -- | /NB:/ Setting the value of this 'AffineTraversal' will only set the value
  -- in 'at' if it is already present.
  --
  -- If you want to be able to insert /missing/ values, you want 'at'.
  --
  -- >>> [1,2,3,4] & ix 2 %~ (*10)
  -- [1,2,30,4]
  --
  -- >>> "abcd" & ix 2 .~ 'e'
  -- "abed"
  --
  -- >>> "abcd" ^? ix 2
  -- Just 'c'
  --
  -- >>> [] ^? ix 2
  -- Nothing
  ix :: Index m -> Optic' (IxKind m) NoIx m (IxValue m)
  default ix :: (At m, IxKind m ~ An_AffineTraversal) => Index m -> Optic' (IxKind m) NoIx m (IxValue m)
  ix = ixAt
  {-# INLINE ix #-}

-- | A definition of 'ix' for types with an 'At' instance. This is the default
-- if you don't specify a definition for 'ix'.
ixAt :: At m => Index m -> AffineTraversal' m (IxValue m)
ixAt = \i -> at i % _Just
{-# INLINE ixAt #-}

type instance IxValue (e -> a) = a
instance Eq e => Ixed (e -> a) where
  type IxKind (e -> a) = A_Lens
  ix e = lensVL $ \p f -> p (f e) <&> \a e' -> if e == e' then a else f e'
  {-# INLINE ix #-}

type instance IxValue (Maybe a) = a
instance Ixed (Maybe a) where
  ix () = castOptic @An_AffineTraversal _Just
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
  type IxKind (Identity a) = An_Iso
  ix () = coerced
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
-- arr 'Array.!' i ≡ arr 'Optics.Operators.^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i 'Optics.Operators..~' e '$' arr
-- @
instance Ix i => Ixed (Array.Array i e) where
  ix i = atraversalVL $ \point f arr ->
    if inRange (Array.bounds arr) i
    then f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    else point arr
  {-# INLINE ix #-}

type instance IxValue (UArray i e) = e
-- |
-- @
-- arr 'Array.!' i ≡ arr 'Optics.Operators.^.' 'ix' i
-- arr '//' [(i,e)] ≡ 'ix' i 'Optics.Operators..~' e '$' arr
-- @
instance (Array.IArray UArray e, Ix i) => Ixed (UArray i e) where
  ix i = atraversalVL $ \point f arr ->
    if inRange (Array.bounds arr) i
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
class (Ixed m, IxKind m ~ An_AffineTraversal) => At m where
  -- |
  -- >>> Map.fromList [(1,"world")] ^. at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ Usage of this function might introduce space leaks if you're not
  -- careful to make sure that values put inside the 'Just' constructor are
  -- evaluated. To force the values and avoid such leaks, use 'at'' instead.
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not
  -- 'Array'-like ones, where you cannot satisfy the 'Lens' laws.
  at :: Index m -> Lens' m (Maybe (IxValue m))

-- | Version of 'at' strict in the value inside the `Just` constructor.
--
-- Example:
--
-- >>> (at () .~ Just (error "oops") $ Nothing) `seq` ()
-- ()
--
-- >>> (at' () .~ Just (error "oops") $ Nothing) `seq` ()
-- *** Exception: oops
-- ...
--
-- >>> view (at ()) (Just $ error "oops") `seq` ()
-- ()
--
-- >>> view (at' ()) (Just $ error "oops") `seq` ()
-- *** Exception: oops
-- ...
--
-- It also works as expected for other data structures:
--
-- >>> (at 1 .~ Just (error "oops") $ Map.empty) `seq` ()
-- ()
--
-- >>> (at' 1 .~ Just (error "oops") $ Map.empty) `seq` ()
-- *** Exception: oops
-- ...
at' :: At m => Index m -> Lens' m (Maybe (IxValue m))
at' k = at k % iso f f
  where
    f = \case
      Just !x -> Just x
      Nothing -> Nothing
{-# INLINE at' #-}

-- | Delete the value associated with a key in a 'Map'-like container
--
-- @
-- 'sans' k = 'at' k 'Optics.Operators..~' Nothing
-- @
sans :: At m => Index m -> m -> m
sans k = set (at k) Nothing
{-# INLINE sans #-}

instance At (Maybe a) where
  at () = lensVL id
  {-# INLINE at #-}

instance At (IntMap a) where
  at k = lensVL $ \f -> IntMap.alterF f k
  {-# INLINE at #-}

instance Ord k => At (Map k a) where
  at k = lensVL $ \f -> Map.alterF f k
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

-- $setup
-- >>> import Optics.Core
