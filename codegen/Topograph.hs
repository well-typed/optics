{-# LANGUAGE RankNTypes, ScopedTypeVariables, RecordWildCards #-}
-- | Copyright: (c) 2018, Oleg Grenrus
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Tools to work with Directed Acyclic Graphs,
-- by taking advantage of topological sorting.
--
module Topograph (
    -- * Graph
    -- $setup

    G (..),
    runG,
    runG',
    -- * All paths
    allPaths,
    allPaths',
    allPathsTree,
    -- * DFS
    dfs,
    dfsTree,
    -- * Longest path
    longestPathLengths,
    -- * Transpose
    transpose,
    -- * Transitive reduction
    reduction,
    -- * Transitive closure
    closure,
    -- * Query
    edgesSet,
    adjacencyMap,
    adjacencyList,
    -- * Helper functions
    treePairs,
    pairs,
    getDown,
    ) where

import           Control.Monad.ST            (ST, runST)
import           Data.Maybe                  (fromMaybe, catMaybes, mapMaybe)
import           Data.Monoid                 (First (..))
import           Data.List                   (sort)
import           Data.Foldable               (for_)
import           Data.Ord                    (Down (..))
import qualified Data.Graph                  as G
import           Data.Tree                   as T
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as MU

import Debug.Trace

-- | Graph representation.
data G v a = G
    { gVertices     :: [a]             -- ^ all vertices, in topological order.
    , gFromVertex   :: a -> v          -- ^ retrieve original vertex data. /O(1)/
    , gToVertex     :: v -> Maybe a    -- ^ /O(log n)/
    , gEdges        :: a -> [a]        -- ^ Outgoing edges.
    , gDiff         :: a -> a -> Int   -- ^ Upper bound of the path length. Negative if there aren't path. /O(1)/
    , gVerticeCount :: Int
    , gToInt        :: a -> Int
    }

-- | Run action on topologically sorted representation of the graph.
--
-- === __Examples__
--
-- ==== Topological sorting
--
-- >>> runG example $ \G {..} -> map gFromVertex gVertices
-- Right "axbde"
--
-- Vertices are sorted
--
-- >>> runG example $ \G {..} -> map gFromVertex $ sort gVertices
-- Right "axbde"
--
-- ==== Outgoing edges
--
-- >>> runG example $ \G {..} -> map (map gFromVertex . gEdges) gVertices
-- Right ["xbde","de","d","e",""]
--
-- Note: edges are always larger than source vertex:
--
-- >>> runG example $ \G {..} -> getAll $ foldMap (\a -> foldMap (\b -> All (a < b)) (gEdges a)) gVertices
-- Right True
--
-- ==== Not DAG
--
-- >>> let loop = M.map S.fromList $ M.fromList [('a', "bx"), ('b', "cx"), ('c', "ax"), ('x', "")]
-- >>> runG loop $ \G {..} -> map gFromVertex gVertices
-- Left "abc"
--
-- >>> runG (M.singleton 'a' (S.singleton 'a')) $ \G {..} -> map gFromVertex gVertices
-- Left "aa"
--
runG
    :: forall v r. Ord v
    => Map v (Set v)                    -- ^ Adjacency Map
    -> (forall i. Ord i => G v i -> r)  -- ^ function on linear indices
    -> Either [v] r                     -- ^ Return the result or a cycle in the graph.
runG m f
    | Just l <- loop = Left (map (indices V.!) l)
    | otherwise      = Right (f g)
  where
    gr :: G.Graph
    r  :: G.Vertex -> ((), v, [v])
    _t  :: v -> Maybe G.Vertex

    (gr, r, _t) = G.graphFromEdges [ ((), v, S.toAscList us) | (v, us) <- M.toAscList m ]

    r' :: G.Vertex -> v
    r' i = case r i of (_, v, _) -> v

    topo :: [G.Vertex]
    topo = G.topSort gr

    indices :: V.Vector v
    indices = V.fromList (map r' topo)

    revIndices :: Map v Int
    revIndices = M.fromList $ zip (map r' topo) [0..]

    edges :: V.Vector [Int]
    edges = V.map
        (\v -> maybe
            []
            (\sv -> sort $ mapMaybe (\v' -> M.lookup v' revIndices) $ S.toList sv)
            (M.lookup v m))
        indices

    -- TODO: let's see if this check is too expensive
    loop :: Maybe [Int]
    loop = getFirst $ foldMap (\a -> foldMap (check a) (gEdges g a)) (gVertices g)
      where
        check a b
            | a < b     = First Nothing
            -- TODO: here we could use shortest path
            | otherwise = First $ case allPaths g b a of
                []      -> Nothing
                (p : _) -> Just p

    g :: G v Int
    g = G
        { gVertices     = [0 .. V.length indices - 1]
        , gFromVertex   = (indices V.!)
        , gToVertex     = (`M.lookup` revIndices)
        , gDiff         = \a b -> b - a
        , gEdges        = (edges V.!)
        , gVerticeCount = V.length indices
        , gToInt        = id
        }

-- | Like 'runG' but returns 'Maybe'
runG'
    :: forall v r. Ord v
    => Map v (Set v)                    -- ^ Adjacency Map
    -> (forall i. Ord i => G v i -> r)  -- ^ function on linear indices
    -> Maybe r                          -- ^ Return the result or 'Nothing' if there is a cycle.
runG' m f = either (const Nothing) Just (runG m f)

-------------------------------------------------------------------------------
-- All paths
-------------------------------------------------------------------------------

-- | All paths from @a@ to @b@. Note that every path has at least 2 elements, start and end.
-- Use 'allPaths'' for the intermediate steps only.
--
-- >>> runG example $ \g@G{..} -> fmap3 gFromVertex $ allPaths g <$> gToVertex 'a' <*> gToVertex 'e'
-- Right (Just ["axde","axe","abde","ade","ae"])
--
-- >>> runG example $ \g@G{..} -> fmap3 gFromVertex $ allPaths g <$> gToVertex 'a' <*> gToVertex 'a'
-- Right (Just [])
--
allPaths :: forall v a. Ord a => G v a -> a -> a -> [[a]]
allPaths g a b = map (\p -> a : p) (allPaths' g a b [b])

-- | 'allPaths' without begin and end elements.
--
-- >>> runG example $ \g@G{..} -> fmap3 gFromVertex $ allPaths' g <$> gToVertex 'a' <*> gToVertex 'e' <*> pure []
-- Right (Just ["xd","x","bd","d",""])
--
allPaths' :: forall v a. Ord a => G v a -> a -> a -> [a] -> [[a]]
allPaths' G {..} a b end = concatMap go (gEdges a) where
    go :: a -> [[a]]
    go i
        | i == b    = [end]
        | otherwise =
            let js :: [a]
                js = filter (<= b) $ gEdges i

                js2b :: [[a]]
                js2b = concatMap go js

            in map (i:) js2b



-- | Like 'allPaths' but return a 'T.Tree'.
--
-- >>> let t = runG example $ \g@G{..} -> fmap3 gFromVertex $ allPathsTree g <$> gToVertex 'a' <*> gToVertex 'e'
-- >>> fmap3 (T.foldTree $ \a bs -> if null bs then [[a]] else concatMap (map (a:)) bs) t
-- Right (Just (Just ["axde","axe","abde","ade","ae"]))
--
-- >>> fmap3 (S.fromList . treePairs) t
-- Right (Just (Just (fromList [('a','b'),('a','d'),('a','e'),('a','x'),('b','d'),('d','e'),('x','d'),('x','e')])))
--
-- >>> let ls = runG example $ \g@G{..} -> fmap3 gFromVertex $ allPaths g <$> gToVertex 'a' <*> gToVertex 'e'
-- >>> fmap2 (S.fromList . concatMap pairs) ls
-- Right (Just (fromList [('a','b'),('a','d'),('a','e'),('a','x'),('b','d'),('d','e'),('x','d'),('x','e')]))
--
-- >>> traverse3_ dispTree t
-- 'a'
--   'x'
--     'd'
--       'e'
--     'e'
--   'b'
--     'd'
--       'e'
--   'd'
--     'e'
--   'e'
--
-- >>> traverse3_ (putStrLn . T.drawTree . fmap show) t
-- 'a'
-- |
-- +- 'x'
-- |  |
-- |  +- 'd'
-- |  |  |
-- |  |  `- 'e'
-- |  |
-- |  `- 'e'
-- ...
--
allPathsTree :: forall v a. Ord a => G v a -> a -> a -> Maybe (T.Tree a)
allPathsTree G {..} a b = go a where
    go :: a -> Maybe (T.Tree a)
    go i
        | i == b    = Just (T.Node b [])
        | otherwise = case mapMaybe go $ filter (<= b) $ gEdges i of
            [] -> Nothing
            js -> Just (T.Node i js)

-------------------------------------------------------------------------------
-- DFS
-------------------------------------------------------------------------------

-- | Depth-first paths starting at a vertex.
--
-- >>> runG example $ \g@G{..} -> fmap3 gFromVertex $ dfs g <$> gToVertex 'x'
-- Right (Just ["xde","xe"])
--
dfs :: forall v a. Ord a => G v a -> a -> [[a]]
dfs G {..} = go where
    go :: a -> [[a]]
    go a = case gEdges a of
        [] -> [[a]]
        bs -> concatMap (\b -> map (a :) (go b)) bs

-- | like 'dfs' but returns a 'T.Tree'.
--
-- >>> traverse2_ dispTree $ runG example $ \g@G{..} -> fmap2 gFromVertex $ dfsTree g <$> gToVertex 'x'
-- 'x'
--   'd'
--     'e'
--   'e'
dfsTree :: forall v a. Ord a => G v a -> a -> T.Tree a
dfsTree G {..} = go where
    go :: a -> Tree a
    go a = case gEdges a of
        [] -> T.Node a []
        bs -> T.Node a $ map go bs

-------------------------------------------------------------------------------
-- Longest / shortest path
-------------------------------------------------------------------------------

-- | Longest paths lengths starting from a vertex.
--
-- >>> runG example $ \g@G{..} -> longestPathLengths g <$> gToVertex 'a'
-- Right (Just [0,1,1,2,3])
--
-- >>> runG example $ \G {..} -> map gFromVertex gVertices
-- Right "axbde"
--
-- >>> runG example $ \g@G{..} -> longestPathLengths g <$> gToVertex 'b'
-- Right (Just [0,0,0,1,2])
--
longestPathLengths :: Ord a => G v a -> a -> [Int]
longestPathLengths = pathLenghtsImpl max

-- | Shortest paths lengths starting from a vertex.
--
-- >>> runG example $ \g@G{..} -> shortestPathLengths g <$> gToVertex 'a'
-- Right (Just [0,1,1,1,1])
--
-- >>> runG example $ \g@G{..} -> shortestPathLengths g <$> gToVertex 'b'
-- Right (Just [0,0,0,1,2])
--
shortestPathLengths :: Ord a => G v a -> a -> [Int]
shortestPathLengths = pathLenghtsImpl min' where
    min' 0 y = y
    min' x y = min x y

pathLenghtsImpl :: forall v a. Ord a => (Int -> Int -> Int) -> G v a -> a -> [Int]
pathLenghtsImpl merge G {..} a = runST $ do
    v <- MU.replicate (length gVertices) (0 :: Int)
    go v (S.singleton a)
    v' <- U.freeze v
    pure (U.toList v')
  where
    go :: MU.MVector s Int -> Set a -> ST s ()
    go v xs = do
        case S.minView xs of
            Nothing       -> pure ()
            Just (x, xs') -> do
                c <- MU.unsafeRead v (gToInt x)
                let ys = S.fromList $ gEdges x
                for_ ys $ \y ->
                    flip (MU.unsafeModify v) (gToInt y) $ \d -> merge d (c + 1)
                go v (xs' `S.union` ys)

-------------------------------------------------------------------------------
-- Transpose
-------------------------------------------------------------------------------

-- | Graph with all edges reversed.
--
-- >>> runG example $ adjacencyList . transpose
-- Right [('a',""),('b',"a"),('d',"abx"),('e',"adx"),('x',"a")]
--
-- === __Properties__
--
-- Commutes with 'closure'
--
-- >>> runG example $ adjacencyList . closure . transpose
-- Right [('a',""),('b',"a"),('d',"abx"),('e',"abdx"),('x',"a")]
--
-- >>> runG example $ adjacencyList . transpose . closure
-- Right [('a',""),('b',"a"),('d',"abx"),('e',"abdx"),('x',"a")]
--
-- Commutes with 'reduction'
--
-- >>> runG example $ adjacencyList . reduction . transpose
-- Right [('a',""),('b',"a"),('d',"bx"),('e',"d"),('x',"a")]
--
-- >>> runG example $ adjacencyList . transpose . reduction
-- Right [('a',""),('b',"a"),('d',"bx"),('e',"d"),('x',"a")]
--
transpose :: forall v a. Ord a => G v a -> G v (Down a)
transpose G {..} = G
    { gVertices     = map Down $ reverse gVertices
    , gFromVertex   = gFromVertex . getDown
    , gToVertex     = fmap Down . gToVertex
    , gEdges        = gEdges'
    , gDiff         = \(Down a) (Down b) -> gDiff b a
    , gVerticeCount = gVerticeCount
    , gToInt        = \(Down a) -> gVerticeCount - gToInt a - 1
    }
  where
    gEdges' :: Down a -> [Down a]
    gEdges' (Down a) = es V.! gToInt a

    -- Note: in original order!
    es :: V.Vector [Down a]
    es = V.fromList $ map (map Down . revEdges) gVertices

    revEdges :: a -> [a]
    revEdges x = concatMap (\y -> [y | x `elem` gEdges y ]) gVertices


-------------------------------------------------------------------------------
-- Reduction
-------------------------------------------------------------------------------

-- | Transitive reduction.
--
-- Smallest graph,
-- such that if there is a path from /u/ to /v/ in the original graph,
-- then there is also such a path in the reduction.
--
-- >>> runG example $ \g -> adjacencyList $ reduction g
-- Right [('a',"bx"),('b',"d"),('d',"e"),('e',""),('x',"d")]
--
-- Taking closure first doesn't matter:
--
-- >>> runG example $ \g -> adjacencyList $ reduction $ closure g
-- Right [('a',"bx"),('b',"d"),('d',"e"),('e',""),('x',"d")]
--
reduction :: Ord a => G v a -> G v a
reduction = transitiveImpl (== 1)

-------------------------------------------------------------------------------
-- Closure
-------------------------------------------------------------------------------

-- | Transitive closure.
--
-- A graph,
-- such that if there is a path from /u/ to /v/ in the original graph,
-- then there is an edge from /u/ to /v/ in the closure.
--
-- >>> runG example $ \g -> adjacencyList $ closure g
-- Right [('a',"bdex"),('b',"de"),('d',"e"),('e',""),('x',"de")]
--
-- Taking reduction first, doesn't matter:
--
-- >>> runG example $ \g -> adjacencyList $ closure $ reduction g
-- Right [('a',"bdex"),('b',"de"),('d',"e"),('e',""),('x',"de")]
--
closure :: Ord a => G v a -> G v a
closure = transitiveImpl (/= 0)

transitiveImpl :: forall v a. Ord a => (Int -> Bool) -> G v a -> G v a
transitiveImpl pred g@G {..} = g { gEdges = gEdges' } where
    gEdges' :: a -> [a]
    gEdges' a = es V.! gToInt a

    es :: V.Vector [a]
    es = V.fromList $ map f gVertices where

    f :: a -> [a]
    f x = catMaybes $ zipWith edge gVertices (longestPathLengths g x)

    edge y i | pred i    = Just y
             | otherwise = Nothing

-------------------------------------------------------------------------------
-- Display
-------------------------------------------------------------------------------

-- | Recover adjacency map representation from the 'G'.
--
-- >>> runG example adjacencyMap
-- Right (fromList [('a',fromList "bdex"),('b',fromList "d"),('d',fromList "e"),('e',fromList ""),('x',fromList "de")])
adjacencyMap :: Ord v => G v a -> Map v (Set v)
adjacencyMap G {..} = M.fromList $ map f gVertices where
    f x = (gFromVertex x, S.fromList $ map gFromVertex $ gEdges x)

-- | Adjacency list representation of 'G'.
--
-- >>> runG example adjacencyList
-- Right [('a',"bdex"),('b',"d"),('d',"e"),('e',""),('x',"de")]
adjacencyList :: Ord v => G v a -> [(v, [v])]
adjacencyList = flattenAM . adjacencyMap

flattenAM :: Map a (Set a) -> [(a, [a])]
flattenAM = map (fmap S.toList) . M.toList

-- |
--
-- >>> runG example $ \g@G{..} -> map (\(a,b) -> [gFromVertex a, gFromVertex b]) $  S.toList $ edgesSet g
-- Right ["ax","ab","ad","ae","xd","xe","bd","de"]
edgesSet :: Ord a => G v a -> Set (a, a)
edgesSet G {..} = S.fromList
    [ (x, y)
    | x <- gVertices
    , y <- gEdges x
    ]

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Like 'pairs' but for 'T.Tree'.
treePairs :: Tree a -> [(a,a)]
treePairs (T.Node i js) =
    [ (i, j) | T.Node j _ <- js ] ++ concatMap treePairs js

-- | Consequtive pairs.
--
-- >>> pairs [1..10]
-- [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
--
-- >>> pairs []
-- []
--
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

-- | Unwrap 'Down'.
getDown :: Down a -> a
getDown (Down a) = a

-------------------------------------------------------------------------------
-- Setup
-------------------------------------------------------------------------------

-- $setup
--
-- Graph used in examples (with all arrows pointing down)
--
-- @
--      a -----
--    / | \\    \\
--  b   |   x   \\
--    \\ | /   \\  |
--      d      \\ |
--       ------- e
-- @
--
-- See <https://en.wikipedia.org/wiki/Transitive_reduction> for a picture.
--
-- >>> let example :: Map Char (Set Char); example = M.map S.fromList $ M.fromList [('a', "bxde"), ('b', "d"), ('x', "de"), ('d', "e"), ('e', "")]
--
-- >>> :set -XRecordWildCards
-- >>> import Data.Monoid (All (..))
-- >>> import Data.Foldable (traverse_)
--
-- >>> let fmap2 = fmap . fmap
-- >>> let fmap3 = fmap . fmap2
-- >>> let traverse2_ = traverse_ . traverse_
-- >>> let traverse3_ = traverse_ . traverse2_
--
-- >>> let dispTree :: Show a => Tree a -> IO (); dispTree = go 0 where go i (T.Node x xs) = putStrLn (replicate (i * 2) ' ' ++ show x) >> traverse_ (go (succ i)) xs
