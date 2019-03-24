module Main (main) where

import           System.Environment   (getArgs)

import           MetaMetaPost         (printDiagram)

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
        _                     -> printDiagram hierarchy
