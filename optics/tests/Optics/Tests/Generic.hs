{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Generic where

import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Optics
import Optics.Tests.Utils

-- | A type with a deep generic representation.
data Big = Big
  { f1  :: Int, f2  :: Int, f3  :: Int, f4  :: Int, f5  :: Int
  , f6  :: Int, f7  :: Int, f8  :: Int, f9  :: Int, f10 :: Int
  , f11 :: Int, f12 :: Int, f13 :: Int, f14 :: Int, f15 :: Int
  , f16 :: Int, f17 :: Int, f18 :: Int, f19 :: Int, f20 :: Int
  , f21 :: Int, f22 :: Int, f23 :: Int, f24 :: Int, f25 :: Int
  , f26 :: Int, f27 :: Int, f28 :: Int, f29 :: Int, f30 :: Int
  , f31 :: Int, f32 :: Int, f33 :: Int, f34 :: Int, f35 :: Int
  , f36 :: Int, f37 :: Int, f38 :: Int, f39 :: Int, f40 :: Int
  , f41 :: Int, f42 :: Int, f43 :: Int, f44 :: Int, f45 :: Int
  , f46 :: Int, f47 :: Int, f48 :: Int, f49 :: Int, f50 :: Int
  , f51 :: Int, f52 :: Int, f53 :: Int, f54 :: Int, f55 :: Int
  , f56 :: Int, f57 :: Int, f58 :: Int, f59 :: Int, f60 :: Int
  , f61 :: Int, f62 :: Int, f63 :: Int, f64 :: Int, f65 :: Int
  , f66 :: Int, f67 :: Int, f68 :: Int, f69 :: Int, f70 :: Int
  , f71 :: Int, f72 :: Int, f73 :: Int, f74 :: Int, f75 :: Int
  , f76 :: Int, f77 :: Int, f78 :: Int, f79 :: Int, f80 :: Int
  , f81 :: Int, f82 :: Int, f83 :: Int, f84 :: Int, f85 :: Int
  , f86 :: Int, f87 :: Int, f88 :: Int, f89 :: Int, f90 :: Int
  , f91 :: Int, f92 :: Int, f93 :: Int, f94 :: Int, f95 :: Int
  , f96 :: Int, f97 :: Int, f98 :: Int, f99 :: Int, f100 :: Int
  }
  deriving Generic

genericTests :: TestTree
genericTests = testGroup "Generic optics"
  [ testCase "view (gfield @\"f57\") s = f57 s" $
    assertSuccess $(inspectTest $ 'big1lhs ==- 'big1rhs)
  , testCase "set (gfield @\"f57\") b s = s { f57 = b }" $
    assertSuccess $(inspectTest $ 'big2lhs ==- 'big2rhs)
  , testCase "preview (gafield @\"f57\") s = Just (f57 s)" $
    assertSuccess $(inspectTest $ 'big3lhs ==- 'big3rhs)
  , testCase "set (gafield @\"f57\") b s = s { f57 = b }" $
    assertSuccess $(inspectTest $ 'big4lhs ==- 'big4rhs)
  , testCase "view (gposition @57) s = f57 s" $
    assertSuccess $(inspectTest $ 'big5lhs ==- 'big5rhs)
  , testCase "set (gposition @57) b s = s { f57 = b }" $
    assertSuccess $(inspectTest $ 'big6lhs ==- 'big6rhs)
  , testCase "view #f57 s = f57 s" $
    assertSuccess $(inspectTest $ 'big7lhs ==- 'big1rhs)
  , testCase "set #f57 b s = s { f57 = b }" $
    assertSuccess $(inspectTest $ 'big8lhs ==- 'big2rhs)
  , testCase "preview #?f57 s = Just (f57 s)" $
    assertSuccess $(inspectTest $ 'big9lhs ==- 'big3rhs)
  , testCase "set #?f57 b s = s { f57 = b }" $
    assertSuccess $(inspectTest $ 'big10lhs ==- 'big4rhs)
  , testCase "gafield infers the result type of a type changing update" $
    assertEqual "result" "R {er = 'c'}" eitherUpdate
  ]

data Either' a b = L { el :: a } | R { er :: b }
  deriving (Show, Generic)

-- | The result type is inferred only from the type of the source.
eitherUpdate :: String
eitherUpdate = show (set (gafield @"el") True (R 'c' :: Either' Int Char))

big1lhs, big1rhs :: Big -> Int
big1lhs s = view (gfield @"f57") s
big1rhs Big{f57} = f57

big2lhs, big2rhs :: Big -> Int -> Big
big2lhs s b = set (gfield @"f57") b s
big2rhs s b = s { f57 = b }

big3lhs, big3rhs :: Big -> Maybe Int
big3lhs s = preview (gafield @"f57") s
big3rhs s = Just (f57 s)

big4lhs, big4rhs :: Big -> Int -> Big
big4lhs s b = set (gafield @"f57") b s
big4rhs s b = s { f57 = b }

big5lhs, big5rhs :: Big -> Int
big5lhs s = view (gposition @57) s
big5rhs Big{f57} = f57

big6lhs, big6rhs :: Big -> Int -> Big
big6lhs s b = set (gposition @57) b s
big6rhs s b = s { f57 = b }

big7lhs :: Big -> Int
big7lhs s = view #f57 s

big8lhs :: Big -> Int -> Big
big8lhs s b = set #f57 b s

big9lhs :: Big -> Maybe Int
#if __GLASGOW_HASKELL__ >= 906
big9lhs s = preview #"?f57" s
#else
big9lhs s = preview (gafield @"f57") s
#endif

big10lhs :: Big -> Int -> Big
#if __GLASGOW_HASKELL__ >= 906
big10lhs s b = set #"?f57" b s
#else
big10lhs s b = set (gafield @"f57") b s
#endif
