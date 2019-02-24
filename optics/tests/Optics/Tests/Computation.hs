{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Computation (computationTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Optics
import Optics.Tests.Utils

computationTests :: TestTree
computationTests = testGroup "computation"
  [ testGroup "Lens"
    [ testCase "view (lens f g) = f" $
      assertSuccess $(inspectTest $ 'lens1lhs === 'lens1rhs)
    , testCase "set (lens f g) = g" $
      assertSuccess $(inspectTest $ 'lens2lhs === 'lens2rhs)
    ]
  , testGroup "AffineTraversal"
    -- this doesn't hold definitionally: we need law here
    [ testCase "withAffineTraversal (atraversal f g) (\\ _ g' -> g') /= g" $
      assertFailure' $(inspectTest $ 'atraversal1lhs === 'atraversal1rhs)
    , testCase "withAffineTraversal (atraversal f g) (\\ _ g' -> g') = ..." $
      assertSuccess $(inspectTest $ 'atraversal1lhs === 'atraversal1rhs_)
    , testCase "withAffineTraversal (atraversal f g) (\\ f' _ -> f') = f" $
      assertSuccess $(inspectTest $ 'atraversal2lhs === 'atraversal2rhs)
    ]
  , testGroup "AffineFold"
    [ testCase "preview (afolding f) = f" $
      assertSuccess $(inspectTest $ 'afold1lhs === 'afold1rhs)
    ]
  , testGroup "Setter"
    [ testCase "over (sets f) = f" $
      assertSuccess $(inspectTest $ 'setter1lhs === 'setter1rhs)
    ]
  ]

lens1lhs, lens1rhs :: (s -> a) -> (s -> a -> s) -> (s -> a)
lens1lhs f g s = view (lens f g) s
lens1rhs f _ s = f s

lens2lhs, lens2rhs :: (s -> a) -> (s -> b -> t) -> (s -> b -> t)
lens2lhs f g s b = set (lens f g) b s
lens2rhs _ g s b = g s b

atraversal1lhs, atraversal1rhs, atraversal1rhs_
  :: (s -> Either t a) -> (s -> b -> t) -> (s -> b -> t)
atraversal1lhs f g s b = withAffineTraversal (atraversal f g) (\_ g' -> g') s b
atraversal1rhs _ g s b = g s b
atraversal1rhs_ f g s b = either id (\_ -> g s b) (f s)

atraversal2lhs, atraversal2rhs
  :: (s -> Either t a) -> (s -> b -> t) -> (s -> Either t a)
atraversal2lhs f g s = withAffineTraversal (atraversal f g) (\f' _ -> f') s
atraversal2rhs f _ s = f s

afold1lhs, afold1rhs :: (s -> Maybe a) -> s -> Maybe a
afold1lhs sma s = preview (afolding sma) s
afold1rhs sma s = sma s

setter1lhs, setter1rhs :: ((a -> b) -> s -> t) -> ((a -> b) -> s -> t)
setter1lhs f ab s = over (sets f) ab s
setter1rhs f ab s = f ab s
