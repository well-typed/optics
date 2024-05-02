{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (unless)
import Data.Foldable (for_, fold)
import Data.List (intersect)
import Data.List (intersect)
import Data.Map.Strict (Map)
import Data.Set (Set)
import System.Environment (getArgs)

import Topograph

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Maybe (listToMaybe)

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------

-- | Optic kinds.
data OpticKind
  -- | Tag for an iso.
  =  An_Iso
  -- | Tag for a lens.
  |  A_Lens
  -- | Tag for a prism.
  |  A_Prism
  -- | Tag for an affine traversal.
  |  An_AffineTraversal
  -- | Tag for a traversal.
  |  A_Traversal
  -- | Tag for a setter.
  |  A_Setter
  -- | Tag for a reversed prism.
  |  A_ReversedPrism
  -- | Tag for a getter.
  |  A_Getter
  -- | Tag for an affine fold.
  |  An_AffineFold
  -- | Tag for a non-empty fold.
  |  A_NeFold
  -- | Tag for a fold.
  |  A_Fold
  -- | Tag for a reversed lens.
  |  A_ReversedLens
  -- | Tag for a review.
  |  A_Review
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Adjancency map
opticsKind :: Map OpticKind (Set OpticKind)
opticsKind = mkProper $ Map.fromListWith (<>)
    [ An_Iso             ~> A_Lens
    , An_Iso             ~> A_Prism
    , An_Iso             ~> A_ReversedLens
    , An_Iso             ~> A_ReversedPrism
    , A_ReversedPrism    ~> A_Getter
    , A_ReversedLens     ~> A_Review
    , A_Prism            ~> A_Review
    , A_Prism            ~> An_AffineTraversal
    , A_Lens             ~> An_AffineTraversal
    , An_AffineTraversal ~> A_Traversal
    , A_Traversal        ~> A_Setter

    -- folds
    , A_Lens             ~> A_Getter
    , An_AffineTraversal ~> An_AffineFold
    , A_Traversal        ~> A_Fold

    , A_Getter           ~> An_AffineFold
    , A_Getter           ~> A_NeFold
    , A_NeFold           ~> A_Fold
    , An_AffineFold      ~> A_Fold
    ]
  where
    k ~> k' = (k, Set.singleton k')

    mkProper m = Map.union m (Map.fromSet (const Set.empty) $ fold m)

-------------------------------------------------------------------------------
-- Gen
-------------------------------------------------------------------------------

maxLength :: Int
maxLength = maximum [ length (show k) | k <- [ An_Iso .. maxBound ] ]

leftpad :: String -> String
leftpad s = s ++ replicate (maxLength - length s) ' '

-------------------------------------------------------------------------------
-- join
-------------------------------------------------------------------------------

genJoin :: IO ()
genJoin = either (fail . show) id $ runG opticsKind $ \g -> do
    let G {..} = closure g

    for_ gVertices $ \a -> do
        let k = gFromVertex a
        putStrLn   ""
        putStrLn $ "-- " ++ show k ++ " -----"

        putStrLn $ joinInstance k k k

        for_ gVertices $ \b -> unless (a == b) $ do
            let l = gFromVertex b

            let ke = Set.fromList $ gEdges a
            let le = Set.fromList $ gEdges b

            let mkl | a `elem` le = [a]
                    | b `elem` ke = [b]
                    | otherwise = case Set.maxView $ Set.intersection ke le of
                        Nothing      -> []
                        Just (j, js) -> foldr f [j] js where
                            f u vs = u : filter (`notElem` gEdges u) vs

            case mkl of
                -- alignment is important, thus padding
                []          -> putStrLn $ unwords
                  [ "--                              no JoinKinds"
                  , leftpad (show k)
                  , leftpad (show l)
                  ]
                kls@(_:_:_) -> putStrLn $ "-- error: multiple joins: "
                                       ++ show (map gFromVertex kls)
                [kl]        -> putStrLn . joinInstance k l $ gFromVertex kl
  where
    joinInstance k l m = unwords
      [ "instance k ~"
      , leftpad $ show m
      , "=> JoinKinds"
      , leftpad $ show k
      , leftpad $ show l
      , "k"
      , "where\n  joinKinds r = r"
      ]


-------------------------------------------------------------------------------
-- subtypes
-------------------------------------------------------------------------------

genSubtypes :: IO ()
genSubtypes = either (fail . show) id $ runG opticsKind $ \g -> do
    let G {..} = closure g

    for_ gVertices $ \a -> do
        let k = gFromVertex a

        unless (null $ gEdges a) $ do
            putStrLn $ "-- " ++ show k

            for_ (gEdges a) $ \a' -> do
                let k' = gFromVertex a'

                putStrLn $ unwords
                    [ "instance Is"
                    , leftpad (show k)
                    , leftpad (show k')
                    , "where implies r = r"
                    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        "join" : _ -> genJoin
        _          -> genSubtypes
