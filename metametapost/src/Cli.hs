module Main (main) where

import           Data.List (find)
import           System.Environment   (getArgs)

import           MetaMetaPost         (printDiagram)

import           MMP.Optics.Common
import           MMP.Optics.Hierarchy
import           MMP.Optics.Indexed
import           MMP.Optics.Re

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("hierarchy" : _)     -> printDiagram hierarchy
        ("reoptics" : _)      -> printDiagram reOptics
        ("indexedoptics" : _) -> printDiagram indexedOptics
        (s : _) | Just k <- find ((s ==) . okName) [minBound..maxBound] -> printDiagram (hierarchyFocus k)
        _                     -> printDiagram hierarchy
