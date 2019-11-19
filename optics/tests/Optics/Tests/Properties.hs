module Optics.Tests.Properties (propertiesTests) where

import Data.Either (isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Fun, expectFailure, applyFun, applyFun2, Property, (===), (==>))
import Test.QuickCheck.Poly (A, B, C, OrdA)

import Optics
import Optics.Internal.Optic
import Optics.Internal.Bi

type S = C
type T = OrdA

propertiesTests :: TestTree
propertiesTests = testGroup "properties"
  -- lens bundles /any/ two functions together
  [ testGroup "Lens"
    [ testProperty "view (lens f g) = f" $
      let prop :: Fun S A -> Fun (S, B) T -> S -> Property
          prop f g s =
            view (getting (lens (applyFun f) (applyFun2 g))) s
            ===
            applyFun f s

      in prop
    , testProperty "set (lens f g) = g" $
      let prop :: Fun S A -> Fun (S, B) T -> S -> B -> Property
          prop f g s b =
            set (lens (applyFun f) (applyFun2 g)) b s
            ===
            applyFun2 g s b

      in prop
    ]

  -- also prisms
  , testGroup "Prism"
    [ testProperty "review (prism f g) = f" $
      let prop :: Fun B T -> Fun S (Either T A) -> B -> Property
          prop f g b =
            review (reviewing (castOptic (prism (applyFun f) (applyFun g)))) b
            ===
            applyFun f b
      in prop
    , testProperty "matching (prism f g) = g" $
      let prop :: Fun B T -> Fun S (Either T A) -> S -> Property
          prop f g s =
            matching (prism (applyFun f) (applyFun g)) s
            ===
            applyFun g s
      in prop
    ]

    -- affine traversals are trickier, atraversal doesn't just bundle
    -- two arbitrary functions together.
  , testGroup "AffineTraversal"
    [ testProperty "matching (atraversal f g) = g" $
      let prop :: Fun S (Either T A) -> Fun (S, B) T -> S -> Property
          prop f g s =
            matching (atraversal (applyFun f) (applyFun2 g)) s
            ===
            applyFun f s
      in prop
    , testProperty "set (atraversal f g) ~= flip g" $
      let prop :: Fun S (Either T A) -> Fun (S, B) T -> S -> B -> Property
          prop f g s b =
            set (atraversal (applyFun f) (applyFun2 g)) b s
            ===
            applyFun2 g s b
          in expectFailure prop
    , testProperty "isRight (f s) ==> set (atraversal f g) = flip g" $
      let prop :: Fun S (Either T A) -> Fun (S, B) T -> S -> B -> Property
          prop f g s b =
            isRight (applyFun f s)
            ==>
            set (atraversal (applyFun f) (applyFun2 g)) b s
            ===
            applyFun2 g s b
          in prop
    ]
  ]

reviewing :: Optic A_Review NoIx s t a b -> Review t b
reviewing (Optic o) = Optic (lphantom . o . lphantom)
