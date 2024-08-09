{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Labels.Generic where

import Data.Ord
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Optics
import Optics.Tests.Utils

data Mammal
  = Dog { name :: String, age :: Int }
  | Cat { name :: String, age :: Int, lazy :: Bool }
  deriving (Show, Generic)

data Fish
  = GoldFish { name :: String }
  | Herring  { name :: String }
  deriving (Show, Generic)

data Human a = Human
  { name :: String
  , age  :: Int
  , fish :: Fish
  , pets :: [a]
  }
  deriving (Show, Generic)

----------------------------------------

genericLabelsTests :: TestTree
genericLabelsTests = testGroup "Labels via Generic"
  [
    testCase "view #name s = name s" $
    assertSuccess $(inspectTest $ 'label1lhs ==- 'label1rhs)
  , testCase "set #pets s b = s { pets = b }" $
    assertSuccess $(inspectTest $ 'label2lhs ==- 'label2rhs)
  , testCase "view (#fish % #name) s = name (fish s)" $
    assertSuccess $(inspectTest $ 'label3lhs ==- 'label3rhs)
  , testCase "set (#fish % #name) b s = s { fish = ... }" $
    assertSuccess $(inspectTest $ 'label4lhs ==- 'label4rhs)
  , testCase "set (#pets % traversed % #name) b s = s { pets = ... }" $
    -- GHC 8.2 is the same modulo a case expression structure
    ghc82failure $(inspectTest $ 'label5lhs ==- 'label5rhs)
  , testCase "multiple set with labels = multiple set with record syntax" $
    assertSuccess $(inspectTest $ 'label6lhs ==- 'label6rhs)
  , testCase "optimized petNames (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'petNames)
  , testCase "optimized otherHuman (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'otherHuman)
  , testCase "optimized humanWithFish (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'humanWithFish)
  , testCase "optimized howManyGoldFish (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'howManyGoldFish)
  , testCase "optimized hasLazyPets (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'hasLazyPets)
  , testCase "optimized yearLater (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'yearLater)
  , testCase "optimized oldestPet (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'oldestPet)
  , testCase "optimized luckyDog (generics)" $
    assertSuccess $(inspectTest $ hasNoGenericRep 'luckyDog)
  ]

label1lhs, label1rhs :: forall a. Human a -> String
label1lhs s = view #name s
label1rhs Human{name} = name

label2lhs, label2rhs :: Human a -> [b] -> Human b
label2lhs s b = set #pets b s
label2rhs s b = s { pets = b }

label3lhs, label3rhs :: Human a -> String
label3lhs s = view (#fish % #name) s
label3rhs Human{fish} = case fish of
  GoldFish{name} -> name
  Herring{name}  -> name

label4lhs, label4rhs :: Human a -> String -> Human a
label4lhs s b = set (#fish % #name) b s
label4rhs s b = s { fish = case fish s of
                      GoldFish{} -> GoldFish b
                      Herring{}  -> Herring b
                  }

label5lhs, label5rhs :: Human Mammal -> Bool -> Human Mammal
#if __GLASGOW_HASKELL__ >= 906
label5lhs s b = set (#pets % traversed % #"?lazy") b s
#else
label5lhs s b = set (#pets % traversed % gafield @"lazy") b s
#endif
label5rhs s b = s { pets = (`map` pets s) $ \case
                      Dog name0 age0   -> Dog { name = name0, age = age0 }
                      Cat name0 age0 _ -> Cat { name = name0, age = age0, lazy = b }
                  }

label6lhs, label6rhs :: Human a -> String -> Int -> String -> [b] -> Human b
label6lhs = label6setter
label6rhs s name_ age_ fishName_ pets_ = s
  { name = name_
  , age  = age_
  , fish = case fish s of
      GoldFish{} -> GoldFish fishName_
      herring    -> herring
  , pets = pets_
  }

-- | Check that the setter compiles in full generality.
label6setter
  :: ( Is k1 A_Setter
     , Is k2 A_Setter
     , Is k3 A_Setter
     , Is k4 A_Setter
     , JoinKinds k5 l k4
     , LabelOptic "_GoldFish" l u v a1 b1
     , LabelOptic "age" k2 s1 s2 a2 b2
     , LabelOptic "fish" k5 s2 s3 u v
     , LabelOptic "name" k3 s4 s1 a3 b3
     , LabelOptic "pets" k1 s3 b4 a4 b5
     ) => s4 -> b3 -> b2 -> b1 -> b5 -> b4
label6setter s name_ age_ fishName_ pets_ = s
  & #name              .~ name_
  & #age               .~ age_
  & #fish % #_GoldFish .~ fishName_
  & #pets              .~ pets_

----------------------------------------
-- Basic data manipulation

human :: Human Mammal
human = Human
  { name = "Andrzej"
  , age = 30
  , fish = GoldFish "Goldie"
  , pets = [Dog "Rocky" 3, Cat "Pickle" 4 True, Cat "Max" 1 False]
  }

petNames :: [String]
petNames = toListOf (#pets % folded % #name) human

otherHuman :: Human a
otherHuman = human & set #name "Peter"
                   & set #pets []
                   & set #age  41

humanWithFish :: Human Fish
humanWithFish = set #pets [GoldFish "Goldie", GoldFish "Slick", Herring "See"] human

howManyGoldFish :: Int
howManyGoldFish = lengthOf (#pets % folded % #_GoldFish) humanWithFish

hasLazyPets :: Bool
#if __GLASGOW_HASKELL__ >= 906
hasLazyPets = orOf (#pets % folded % #"?lazy") human
#else
hasLazyPets = orOf (#pets % folded % gafield @"lazy") human
#endif

yearLater :: Human Mammal
yearLater = human & #age %~ (+1)
                  & #pets % mapped % #age %~ (+1)

oldestPet :: Maybe Mammal
oldestPet = maximumByOf (#pets % folded) (comparing $ view #age) human

luckyDog :: Human Mammal
luckyDog = human & set (#pets % mapped % #_Dog % _1) "Lucky"
