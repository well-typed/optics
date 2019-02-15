{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Labels where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection
import qualified GHC.Generics as G

import Optics
import Optics.Generic.Sum.Constructors
import Optics.Tests.Utils

labelsTests :: TestTree
labelsTests = testGroup "Overloaded labels"
  [ testCase "#tint = hand written lens" $
    -- GHC 8.0.2 leaves Generics in lenses for sum types.
    ghc80failure $(inspectTest $ 'tintLabel === 'tintManual)
  , testCase "#_1 = hand written lens" $
    -- GHC 8.0.2 leaves Generics in lenses for sum types.
    ghc80failure $(inspectTest $ 'tintLabelPos === 'tintManual)
  , testCase "#tchar = hand written lens" $
    -- GHC 8.0.2 leaves Generics in lenses for sum types.
    ghc80failure $(inspectTest $ 'tcharLabel === 'tcharManual)
  , testCase "#_2 = hand written lens" $
    -- GHC 8.0.2 leaves Generics in lenses for sum types.
    ghc80failure $(inspectTest $ 'tcharLabelPos === 'tcharManual)
  , testCase "#tfield = hand written lens" $
    assertSuccess $(inspectTest $ 'tfieldLabel ==- 'tfieldManual)
  , testCase "#_1 = hand written lens" $
    assertSuccess $(inspectTest $ 'tfieldLabelPos ==- 'tfieldManual)
  , testCase "#tstring = hand written lens" $
    assertSuccess $(inspectTest $ 'tstringLabel === 'tstringManual)
  , testCase "#_2 = hand written lens" $
    assertSuccess $(inspectTest $ 'tstringLabelPos === 'tstringManual)
  , testCase "#_T31 = hand written prism" $
    assertSuccess $(inspectTest $ 't31Label === 't31Manual)
  , testCase "#_T32 = hand written prism" $
    assertSuccess $(inspectTest $ 't32Label === 't32Manual)
  , testCase "#_T33 = hand written prism" $
    -- Earlier versions don't optimize away hlist to tuple conversion.
    ghc86success $(inspectTest $ 't33Label === 't33Manual)
  ]

data T1
  = T11 { tint :: Int, tchar :: Char }
  | T12 { tint :: Int, tchar :: Char, _tname :: String }
  deriving (Show, G.Generic)

tintLabel, tintLabelPos, tintManual :: Lens' T1 Int
tintLabel    = #tint
tintLabelPos = #_1
tintManual   = lens tint (\t n -> t { tint = n })

tcharLabel, tcharLabelPos, tcharManual :: Lens' T1 Char
tcharLabel    = #tchar
tcharLabelPos = #_2
tcharManual   = lens tchar (\t c -> t { tchar = c })

data T2 a = T2 { tfield :: a, tstring :: String }
  deriving (Show, G.Generic)

tfieldLabel, tfieldLabelPos, tfieldManual :: Lens (T2 a) (T2 b) a b
tfieldLabel    = #tfield
tfieldLabelPos = #_1
tfieldManual   = lens tfield (\t v -> t { tfield = v })

tstringLabel, tstringLabelPos, tstringManual :: Lens' (T2 a) String
tstringLabel    = #tstring
tstringLabelPos = #_2
tstringManual   = lens tstring (\t v -> t { tstring = v })

data T3 a = T31 Int | T32 a | T33 Int String
  deriving (Show, G.Generic)

t31Label, t31Manual :: Prism' (T3 a) Int
#if __GLASGOW_HASKELL >= 802
t31Label = #_T31
#else
t31Label = _Ctor @"T31"
#endif
t31Manual = prism T31 $ \t -> case t of
  T31 a   -> Right a
  T32 _   -> Left t
  T33 _ _ -> Left t

t32Label, t32Manual :: Prism (T3 a) (T3 b) a b
#if __GLASGOW_HASKELL >= 802
t32Label = #_T32
#else
t32Label = _Ctor @"T32"
#endif
t32Manual = prism T32 $ \t -> case t of
  T31 a   -> Left (T31 a)
  T32 a   -> Right a
  T33 a b -> Left (T33 a b)

t33Label, t33Manual :: Prism' (T3 a) (Int, String)
#if __GLASGOW_HASKELL >= 802
t33Label = #_T33
#else
t33Label = _Ctor @"T33"
#endif
t33Manual = prism (uncurry T33) $ \t -> case t of
  T31 _   -> Left t
  T32 _   -> Left t
  T33 a b -> Right (a, b)
