{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Data.Optics
-- Copyright   :  (C) 2012-2016 Edward Kmett, (C) 2006-2012 Neil Mitchell
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  Rank2Types
--
-- Smart and naïve generic traversals given 'Data' instances.
--
-- 'template', 'uniplate', and 'biplate' each build up information about what
-- types can be contained within another type to speed up 'Traversal'.
--
----------------------------------------------------------------------------
module Data.Data.Optics
  (
  -- * Generic Traversal
    template
  , tinplate
  , uniplate
  , biplate
  -- * Data Traversal
  , gtraverse
  ) where

import Data.Data
import Data.HashMap.Strict (HashMap, (!))
import Data.HashSet (HashSet)
import Data.IORef
import Data.Maybe
import Data.Type.Equality ((:~:))
import GHC.Exts (realWorld#)
import GHC.IO
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Optics.Traversal

-------------------------------------------------------------------------------
-- Generic Traversal
-------------------------------------------------------------------------------

-- | A generic applicative transformation that maps over the immediate subterms.
--
-- 'gtraverse' is to 'traverse' what 'gmapM' is to 'mapM'
--
-- This really belongs in @Data.Data@.
gtraverse
  :: (Applicative f, Data a)
  => (forall d. Data d => d -> f d) -> a -> f a
gtraverse f = gfoldl (\x y -> x <*> f y) pure
{-# INLINE gtraverse #-}

-------------------------------------------------------------------------------
-- Naïve Traversal
-------------------------------------------------------------------------------

-- | Naïve 'Traversal' using 'Data'. This does not attempt to optimize the
-- traversal.
--
-- This is primarily useful when the children are immediately obvious, and for
-- benchmarking.
tinplate :: (Data s, Typeable a) => Traversal' s a
tinplate = traversalVL tinplateVL
{-# INLINE tinplate #-}

-- | Internal version of 'tinplate' in van Laarhoven encoding.
tinplateVL :: (Data s, Typeable a) => TraversalVL' s a
tinplateVL f = gfoldl (step f) pure
{-# INLINE tinplateVL #-}

step
  :: forall s a f r. (Applicative f, Typeable a, Data s)
  => (a -> f a) -> f (s -> r) -> s -> f r
step f w s = w <*> case mightBe :: Maybe (s :~: a) of
  Just Refl -> f s
  Nothing   -> tinplateVL f s
{-# INLINE step #-}

-------------------------------------------------------------------------------
-- Smart Traversal
-------------------------------------------------------------------------------

-- | Find every occurrence of a given type @a@ recursively that doesn't require
-- passing through something of type @a@ using 'Data', while avoiding traversal
-- of areas that cannot contain a value of type @a@.
--
-- This is 'uniplate' with a more liberal signature.
template :: forall s a. (Data s, Typeable a) => Traversal' s a
template = traversalVL $ uniplateData (fromOracle answer) where
  answer = hitTest (undefined :: s) (undefined :: a)
{-# INLINE template #-}

-- | Find descendants of type @a@ non-transitively, while avoiding computation
-- of areas that cannot contain values of type @a@ using 'Data'.
--
-- TODO: 'uniplate' is a useful default definition for 'Optics.Plated.plate'
uniplate :: Data a => Traversal' a a
uniplate = template
{-# INLINE uniplate #-}

-- | 'biplate' performs like 'template', except when @s ~ a@, it returns itself
-- and nothing else.
biplate :: forall s a. (Data s, Typeable a) => Traversal' s a
biplate = traversalVL $ biplateData (fromOracle answer) where
  answer = hitTest (undefined :: s) (undefined :: a)
{-# INLINE biplate #-}

-------------------------------------------------------------------------------
-- Type equality
-------------------------------------------------------------------------------

mightBe :: (Typeable a, Typeable b) => Maybe (a :~: b)
mightBe = gcast Refl
{-# INLINE mightBe #-}

-------------------------------------------------------------------------------
-- Data Box
-------------------------------------------------------------------------------

data DataBox = forall a. Data a => DataBox
  { dataBoxKey :: TypeRep
  , _dataBoxVal :: a
  }

dataBox :: Data a => a -> DataBox
dataBox a = DataBox (typeOf a) a
{-# INLINE dataBox #-}

-- partial, caught elsewhere
sybChildren :: Data a => a -> [DataBox]
sybChildren x
  | isAlgType dt = do
    c <- dataTypeConstrs dt
    gmapQ dataBox (fromConstr c `asTypeOf` x)
  | otherwise = []
  where dt = dataTypeOf x
{-# INLINE sybChildren #-}

-------------------------------------------------------------------------------
-- HitMap
-------------------------------------------------------------------------------

type HitMap = HashMap TypeRep (HashSet TypeRep)

emptyHitMap :: HitMap
emptyHitMap = M.fromList
  [ (tRational, S.singleton tInteger)
  , (tInteger,  S.empty)
  ] where
  tRational = typeOf (undefined :: Rational)
  tInteger  = typeOf (undefined :: Integer )

insertHitMap :: DataBox -> HitMap -> HitMap
insertHitMap box hit = fixEq trans (populate box) `mappend` hit where
  populate :: DataBox -> HitMap
  populate a = f a M.empty where
    f (DataBox k v) m
      | M.member k hit || M.member k m = m
      | cs <- sybChildren v = fs cs $ M.insert k (S.fromList $ map dataBoxKey cs) m
    fs []     m = m
    fs (x:xs) m = fs xs (f x m)

  trans :: HitMap -> HitMap
  trans m = M.map f m where
    f x = x `mappend` foldMap g x
    g x = fromMaybe (hit ! x) (M.lookup x m)

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f = go where
  go x | x == x'   = x'
       | otherwise = go x'
       where x' = f x
{-# INLINE fixEq #-}

-- | inlineable 'unsafePerformIO'
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

-------------------------------------------------------------------------------
-- Cache
-------------------------------------------------------------------------------

data Cache = Cache HitMap (HashMap TypeRep (HashMap TypeRep (Maybe Follower)))

cache :: IORef Cache
cache = unsafePerformIO $ newIORef $ Cache emptyHitMap M.empty
{-# NOINLINE cache #-}

readCacheFollower :: DataBox -> TypeRep -> Maybe Follower
readCacheFollower b@(DataBox kb _) ka = inlinePerformIO $
  readIORef cache >>= \ (Cache hm m) -> case M.lookup kb m >>= M.lookup ka of
    Just a -> return a
    Nothing -> E.try (return $! insertHitMap b hm) >>= \r -> case r of
      Left E.SomeException{} ->
        atomicModifyIORef cache $ \(Cache hm' n) ->
          (Cache hm' (insert2 kb ka Nothing n), Nothing)
      Right hm' | fol <- Just (follower kb ka hm') ->
        atomicModifyIORef cache $ \(Cache _ n) ->
          (Cache hm' (insert2 kb ka fol n),    fol)

insert2
  :: TypeRep
  -> TypeRep
  -> a
  -> HashMap TypeRep (HashMap TypeRep a)
  -> HashMap TypeRep (HashMap TypeRep a)
insert2 x y v = M.insertWith (const $ M.insert y v) x (M.singleton y v)
{-# INLINE insert2 #-}

-------------------------------------------------------------------------------
-- Answers
-------------------------------------------------------------------------------

data Answer b a
  = b ~ a => Hit a
  | Follow
  | Miss

-------------------------------------------------------------------------------
-- Oracles
-------------------------------------------------------------------------------

newtype Oracle a = Oracle
  { fromOracle :: forall t. Typeable t => t -> Answer t a }

hitTest :: forall a b. (Data a, Typeable b) => a -> b -> Oracle b
hitTest a b = Oracle $ \(c :: c) ->
  case mightBe :: Maybe (c :~: b) of
    Just Refl -> Hit c
    Nothing   ->
      case readCacheFollower (dataBox a) (typeOf b) of
        Just p | not (p (typeOf c)) -> Miss
        _ -> Follow

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------


biplateData
  :: forall f s a. (Applicative f, Data s)
  => (forall c. Typeable c => c -> Answer c a) -> (a -> f a) -> s -> f s
biplateData o f a0 = go2 a0 where
  go :: Data d => d -> f d
  go s = gfoldl (\x y -> x <*> go2 y) pure s
  go2 :: Data d => d -> f d
  go2 s = case o s of
    Hit a  -> f a
    Follow -> go s
    Miss   -> pure s
{-# INLINE biplateData #-}

uniplateData
  :: forall f s a. (Applicative f, Data s)
  => (forall c. Typeable c => c -> Answer c a) -> (a -> f a) -> s -> f s
uniplateData o f a0 = go a0 where
  go :: Data d => d -> f d
  go s = gfoldl (\x y -> x <*> go2 y) pure s
  go2 :: Data d => d -> f d
  go2 s = case o s of
    Hit a  -> f a
    Follow -> go s
    Miss   -> pure s
{-# INLINE uniplateData #-}

-------------------------------------------------------------------------------
-- Follower
-------------------------------------------------------------------------------

part :: (a -> Bool) -> HashSet a -> (HashSet a, HashSet a)
part p s = (S.filter p s, S.filter (not . p) s)
{-# INLINE part #-}

type Follower = TypeRep -> Bool

follower :: TypeRep -> TypeRep -> HitMap -> Follower
follower a b m
  | S.null hit               = const False
  | S.null miss              = const True
  | S.size hit < S.size miss = (`S.member` hit)
  | otherwise = \k -> not (S.member k miss)
  where (hit, miss) = part (\x -> S.member b (m ! x)) (S.insert a (m ! a))
