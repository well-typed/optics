{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Optics.Tests.Labels where

import Data.Ord
import Control.Monad.Reader

import Optics
import Optics.Operators
import Optics.TH

----------------------------------------
-- Basic data manipulation

data Mammal
  = Dog { mammalName :: String, mammalAge :: Int }
  | Cat { mammalName :: String, mammalAge :: Int, mammalLazy :: Bool }
  deriving Show
makeFieldLabels ''Mammal
makePrismLabels ''Mammal

data Fish = GoldFish | Herring
  deriving Show
makePrismLabels ''Fish

data Human a = Human { humanName :: String, humanAge :: Int, humanPets :: [a] }
  deriving Show
makeFieldLabels ''Human

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
instance LabelOptic "config" An_Equality Config Config Config Config where
  labelOptic = equality

data Env = Env { envConfig :: Config, envRandoms :: [Int] }
makeFieldLabels ''Env

data Nested = Nested { nestedName :: String, nestedEnv :: Env }
makeFieldLabels ''Nested

instance LabelOptic "config" A_Lens Nested Nested Config Config where
  labelOptic = #env % #config

doStuff :: (MonadReader r m, HasConfig k r) => m ()
doStuff = do
  _ <- asks (view #config)
  -- ...
  pure ()

env :: Env
env = Env Config [1..]

-- | Do stuff with 'Config' directly.
doStuffWithConfig :: Monad m => m ()
doStuffWithConfig = runReaderT doStuff Config

-- | Do stuff with larger environment containing 'Config'.
doStuffWithEnv :: Monad m => m ()
doStuffWithEnv = runReaderT doStuff env

-- | Do stuff with even larger environment.
doStuffWithNested :: Monad m => m ()
doStuffWithNested = runReaderT doStuff (Nested "weird" env)
