{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Main (main) where

import Test.Tasty

import Optics
import Optics.Tests.Computation
import Optics.Tests.Core
import Optics.Tests.Eta
import Optics.Tests.Labels
import Optics.Tests.Misc
import Optics.Tests.Properties

-- | Composing a lens and a traversal yields a traversal
_comp1 :: Traversable t => Optic A_Traversal NoIx (t a, y) (t b, y) a b
_comp1 = _1 % traversed

-- | Composing two lenses yields a lens
_comp2 :: Optic A_Lens NoIx ((a, y), y1) ((b, y), y1) a b
_comp2 = _1 % _1

-- | Composing a getter and a lens yields a getter
_comp3 :: Optic A_Getter NoIx ((b, y), b1) ((b, y), b1) b b
_comp3 = to fst % _1

-- | Composing a prism and a lens yields a traversal
_comp4 :: Optic An_AffineTraversal NoIx (Either c (a, y)) (Either c (b, y)) a b
_comp4 = _Right % _1

-- | An iso can be used as a getter
_eg1 :: Int
_eg1 = view (iso (+ 1) (\ x -> x - 1)) 5

-- | A lens can be used as a getter
_eg2 :: (a, b) -> a
_eg2 = view _1

-- These don't typecheck, as one would expect:
--   to fst % mapped  -- Cannot compose a getter with a setter
--   toLens (to fst)  -- Cannot use a getter as a lens

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Inspection"
    [ coreTests
    , etaTests
    , labelsTests
    , miscTests
    ]
  , computationTests
  , propertiesTests
  ]
