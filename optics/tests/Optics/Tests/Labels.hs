{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Optics.Tests.Labels where

import Data.Ord

import Optics
import Optics.Operators
import Optics.TH

data Mammal
  = Dog { mammalName :: String, mammalAge :: Int }
  | Cat { mammalName :: String, mammalAge :: Int, mammalLazy :: Bool }
  deriving Show
makePrisms ''Mammal
makeLabels ''Mammal

data Fish = GoldFish | Herring
  deriving Show
makePrisms ''Fish

data Human a = Human { humanName :: String, humanAge :: Int, humanPets :: [a] }
  deriving Show
makeLabels ''Human

human :: Human Mammal
human = Human
  { humanName = "Andrzej"
  , humanAge = 30
  , humanPets = [Dog "Rocky" 3, Cat "Pickle" 4 True, Cat "Max" 1 False ]
  }

petNames :: [String]
petNames = toListOf (#pets % folded % #name) human

otherHuman :: Human a
otherHuman = human & set #name "Peter"
                   & set #pets []
                   & set #age  41

humanWithFish :: Human Fish
humanWithFish = set #pets [GoldFish, GoldFish, Herring] human

howManyGoldFish :: Int
howManyGoldFish = lengthOf (#pets % folded % _GoldFish) humanWithFish

hasLazyPets :: Bool
hasLazyPets = orOf (#pets % folded % #lazy) human

yearLater :: Human Mammal
yearLater = human & #age %~ (+1)
                  & #pets % mapped % #age %~ (+1)

oldestPet :: Maybe Mammal
oldestPet = maximumByOf (#pets % folded) (comparing $ view #age) human

luckyDog :: Human Mammal
luckyDog = human & set (#pets % mapped % _Dog % _1) "Lucky"
