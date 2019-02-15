{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Computation where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Optics
import Optics.Tests.Utils

computationTests :: TestTree
computationTests = testGroup "computation"
    [ testGroup "Lens"
        [ testCase "view (lens f g) = f" $
            assertSuccess $(inspectTest $ 'compL01 === 'compR01)
        , testCase "set (lens f g) = g" $
            assertSuccess $(inspectTest $ 'compL02 === 'compR02)
        ]
    , testGroup "AffineTraversal"
        -- this doesn't hold definitionally: we need law here
        [ testCase "withAffineTraversal (atraversal f g) (\\ _ g' -> g') /= g" $
             assertFailure' $(inspectTest $ 'compL03 === 'compR03)
        , testCase "withAffineTraversal (atraversal f g) (\\ _ g' -> g') = ..." $
             assertSuccess $(inspectTest $ 'compL03 ==- 'compR03_)
        , testCase "withAffineTraversal (atraversal f g) (\\ f' _ -> f') = f" $
            assertSuccess $(inspectTest $ 'compL04 === 'compR04)
        ]

    , testGroup "Setter"
        [ testCase "over (sets f) = f" $
            assertSuccess $(inspectTest $ 'compL05 === 'compR05)
        ]
    ]

compL01, compR01 :: (s -> a) -> (s -> a -> s) -> (s -> a)
compL01 f g s = view (lens f g) s
compR01 f _ s = f s

compL02, compR02 :: (s -> a) -> (s -> b -> t) -> (s -> b -> t)
compL02 f g s b = set (lens f g) b s
compR02 _ g s b = g s b

compL03, compR03, compR03_ :: (s -> Either t a) -> (s -> b -> t) -> (s -> b -> t)
compL03 f g s b = withAffineTraversal (atraversal f g) (\_ g' -> g') s b
compR03 _ g s b = g s b
compR03_ f g s b = case f s of
    Left t  -> t
    Right _ -> g s b

compL04, compR04 :: (s -> Either t a) -> (s -> b -> t) -> (s -> Either t a)
compL04 f g s = withAffineTraversal (atraversal f g) (\f' _ -> f') s
compR04 f _ s = f s

compL05, compR05 :: ((a -> b) -> s -> t) -> ((a -> b) -> s -> t)
compL05 f ab s = over (sets f) ab s
compR05 f ab s = f ab s
