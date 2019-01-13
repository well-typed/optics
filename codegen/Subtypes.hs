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
      -- | Tag for an equality.
  =  An_Equality
  -- | Tag for an iso.
  |  An_Iso
  -- | Tag for a lens.
  |  A_Lens
  -- | Tag for a prism.
  |  A_Prism
  -- | Tag for an affine traversal.
  |  An_AffineTraversal
  -- | Tag for a traversal.
  |  A_Traversal
  -- | Tag for an indexed traversal.
  |  An_IxTraversal
  -- | Tag for a setter.
  |  A_Setter
  -- | Tag for an indexed setter.
  |  An_IxSetter
  -- | Tag for a prismatic getter.
  |  A_PrismaticGetter
  -- | Tag for a getter.
  |  A_Getter
  -- | Tag for an affine fold.
  |  An_AffineFold
  -- | Tag for a fold.
  |  A_Fold
  -- | Tag for an indexed fold.
  |  An_IxFold
  -- | Tag for a lensy review.
  |  A_LensyReview
  -- | Tag for a review.
  |  A_Review
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Adjancency map
opticsKind :: Map OpticKind (Set OpticKind)
opticsKind = mkProper $ Map.fromListWith (<>)
    [ An_Equality        ~> An_Iso
    , An_Iso             ~> A_Lens
    , An_Iso             ~> A_Prism
    , An_Iso             ~> A_LensyReview
    , An_Iso             ~> A_PrismaticGetter
    , A_PrismaticGetter  ~> A_Getter
    , A_LensyReview      ~> A_Review
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
    , An_AffineFold      ~> A_Fold

    -- indexed
    , A_Traversal        ~> An_IxTraversal
    , A_Setter           ~> An_IxSetter
    , A_Fold             ~> An_IxFold

    , An_IxTraversal     ~> An_IxSetter
    , An_IxTraversal     ~> An_IxFold
    ]
  where
    k ~> k' = (k, Set.singleton k')

    mkProper m = Map.union m (Map.fromSet (const Set.empty) $ fold m)

-------------------------------------------------------------------------------
-- Gen
-------------------------------------------------------------------------------

maxLength :: Int
maxLength = maximum [ length (show k) | k <- [ An_Equality .. maxBound ] ]

leftpad :: String -> String
leftpad s = s ++ replicate (maxLength - length s) ' '

-------------------------------------------------------------------------------
-- join
-------------------------------------------------------------------------------

genJoin :: IO ()
genJoin = either (fail . show) id $ runG opticsKind $ \g -> do
    let G {..} = closure g

    for_ gVertices $ \a -> do
        putStrLn ""

        for_ gVertices $ \b -> unless (a == b) $ do
            let k = gFromVertex a
            let l = gFromVertex b

            let ke = gEdges a
            let le = gEdges b

            -- join, if exists

            -- pick the first element, in topo-order
            -- in arbitrary graph such "join" might not be unique; it is in ours
            --
            -- proper: check that there's path from `x` to all other elements of `xs`
            let pick xs = listToMaybe (gVertices `intersect` xs)

            let mkl | a `elem` le = Just k
                    | b `elem` ke = Just l
                    | otherwise   = fmap gFromVertex $ pick $ le `intersect` ke

            case mkl of
                Nothing -> putStrLn $ "  -- no Join with " ++ show l
                Just kl -> putStrLn $ unwords
                    [ "  Join"
                    , leftpad (show k)
                    , leftpad (show l)
                    , "="
                    , show kl
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
                    , "where implies _ = id"
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
