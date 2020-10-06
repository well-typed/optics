{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Labels.TH where

import Data.Ord
import Data.Word
import Control.Monad.Reader
import Control.Monad.State
import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection
import qualified System.Random as R

import Optics
import Optics.Tests.Utils

data Mammal
  = Dog { mammalName :: String, mammalAge :: Int }
  | Cat { mammalName :: String, mammalAge :: Int, mammalLazy :: Bool }
  deriving Show

data Fish = GoldFish { fishName :: String } | Herring { fishName :: String }
  deriving Show

data Human a = Human
  { humanName :: String
  , humanAge :: Int
  , humanFish :: Fish
  , humanPets :: [a]
  }
  deriving Show

makeFieldLabels ''Mammal
makePrismLabels ''Mammal
makeFieldLabels ''Fish
makePrismLabels ''Fish
makeFieldLabels ''Human

----------------------------------------

thLabelsTests :: TestTree
thLabelsTests = testGroup "Labels via Template Haskell"
  [
    testCase "view #name s = humanName s" $
    assertSuccess $(inspectTest $ 'label1lhs ==- 'label1rhs)
  , testCase "set #pets s b = s { humanPets = b }" $
    assertSuccess $(inspectTest $ 'label2lhs ==- 'label2rhs)
  , testCase "view (#fish % #name) s = fishName (humanFish s)" $
    assertSuccess $(inspectTest $ 'label3lhs ==- 'label3rhs)
  , testCase "set (#fish % #name) b s = s { humanFish = ... }" $
    assertSuccess $(inspectTest $ 'label4lhs ==- 'label4rhs)
  , testCase "multiple set with labels = multiple set with record syntax" $
    assertSuccess $(inspectTest $ 'label5lhs ==- 'label5rhs)
  ]

label1lhs, label1rhs :: Human a -> String
label1lhs s = view #name s
label1rhs s = humanName s

label2lhs, label2rhs :: Human a -> [b] -> Human b
label2lhs s b = set #pets b s
label2rhs s b = s { humanPets = b }

label3lhs, label3rhs :: Human a -> String
label3lhs s = view (#fish % #name) s
label3rhs s = fishName (humanFish s)

label4lhs, label4rhs :: Human a -> String -> Human a
label4lhs s b = set (#fish % #name) b s
label4rhs s b = s { humanFish = (humanFish s) { fishName = b } }

label5lhs, label5rhs :: Human a -> String -> Int -> String -> [b] -> Human b
label5lhs s name_ age_ fishName_ pets_ = s
  & #name         .~ name_
  & #age          .~ age_
  & #fish % #name .~ fishName_
  & #pets         .~ pets_
label5rhs s name_ age_ fishName_ pets_ = s
  { humanName = name_
  , humanAge  = age_
  , humanFish = (humanFish s) { fishName = fishName_ }
  , humanPets = pets_
  }

----------------------------------------
-- Basic data manipulation

human :: Human Mammal
human = Human
  { humanName = "Andrzej"
  , humanAge = 30
  , humanFish = GoldFish "Goldie"
  , humanPets = [Dog "Rocky" 3, Cat "Pickle" 4 True, Cat "Max" 1 False]
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
hasLazyPets = orOf (#pets % folded % #lazy) human

yearLater :: Human Mammal
yearLater = human & #age %~ (+1)
                  & #pets % mapped % #age %~ (+1)

oldestPet :: Maybe Mammal
oldestPet = maximumByOf (#pets % folded) (comparing $ view #age) human

luckyDog :: Human Mammal
luckyDog = human & set (#pets % mapped % #_Dog % _1) "Lucky"

----------------------------------------
-- Generalization of Has* classes

type HasConfig k s = (LabelOptic' "config" k s Config, Is k A_Getter)

data Config = Config
instance
  (k ~ An_Iso, a ~ Config, b ~ Config
  ) => LabelOptic "config" k Config Config a b where
  labelOptic = equality

data Env = Env { envConfig :: Config, envRng :: R.StdGen }
makeFieldLabels ''Env

data Nested = Nested { nestedName :: String, nestedEnv :: Env }
makeFieldLabels ''Nested

instance
  (k ~ A_Lens, a ~ Config, b ~ Config
  ) => LabelOptic "config" k Nested Nested a b where
  labelOptic = #env % #config

doStuff :: (MonadReader r m, HasConfig k r) => m ()
doStuff = do
  _ <- asks (view #config)
  -- ...
  pure ()

env :: Env
env = Env Config (R.mkStdGen 0)

-- | Do stuff with 'Config' directly.
doStuffWithConfig :: Monad m => m ()
doStuffWithConfig = runReaderT doStuff Config

-- | Do stuff with larger environment containing 'Config'.
doStuffWithEnv :: Monad m => m ()
doStuffWithEnv = runReaderT doStuff env

-- | Do stuff with even larger environment.
doStuffWithNested :: Monad m => m ()
doStuffWithNested = runReaderT doStuff (Nested "weird" env)

----------------------------------------
-- Composition

randomValue
  :: (MonadState s m, LabelOptic' "rng" A_Lens s R.StdGen, R.Random r)
  => m r
randomValue = do
  (r, g) <- gets $ view (#rng % to R.random)
  modify' $ set #rng g
  pure r

randomWords :: IO [Word8]
randomWords = do
  rng <- R.mkStdGen <$> R.randomIO
  (`evalStateT` Env Config rng) $ do
    n <- fix $ \loop -> do
      n <- (`mod` 16) <$> randomValue
      if n < 5
        then loop
        else pure n
    replicateM n randomValue
