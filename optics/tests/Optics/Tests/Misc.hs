{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Misc (miscTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection
import qualified Data.Map as M
import qualified Data.Sequence as S

import Optics
import Optics.Tests.Utils

miscTests :: TestTree
miscTests = testGroup "Miscellaneous"
  [ testCase "optimized sipleMapIx" $
    assertSuccess $(inspectTest $ 'simpleMapIx `hasNoTypeClassesExcept` [''Ord])
  , testCase "optimized mapIx" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'mapIx)
  , testCase "optimized seqIx" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'seqIx)
  , testCase "optimized itoList" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'checkitoListOf)
  , testCase "optimized partsOf" $
    ghc80failure $(inspectTest $ hasNoProfunctors 'checkPartsOf)
  , testCase "optimized singular" $
    ghc80failure $(inspectTest $ hasNoProfunctors 'checkSingular)
  , testCase "optimized filteredBy" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'checkFilteredBy)
  , testCase "optimized unsafeFilteredBy" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'checkUnsafeFilteredBy)
    -- GHC <= 8.4 doesn't optimize away profunctor classes
  , testCase "optimized adjoin" $
    ghcLE84failure $(inspectTest $ hasNoProfunctors 'checkAdjoin)
    -- GHC <= 8.4 doesn't optimize away profunctor classes
  , testCase "optimized iadjoin" $
    ghcLE84failure $(inspectTest $ hasNoProfunctors 'checkIxAdjoin)
  , testCase "optimized appendIndices" $
    assertSuccess $ $(inspectTest $ hasNoAppendIndices 'checkNoAppendProof)
  ]

simpleMapIx
  :: Ord k => k -> Either a (M.Map k (b, v)) -> Maybe v
simpleMapIx k = preview (_Right % ix k % _2)

mapIx
  :: (Foldable f, Foldable g, Ord k)
  => (f (Either a (g (M.Map k v))), b) -> k -> [v]
mapIx m k = toListOf (_1 % folded % _Right % folded % ix k) m

seqIx :: Int -> [S.Seq a] -> [a]
seqIx i = toListOf (folded % ix i)

checkitoListOf :: Int -> [S.Seq a] -> [(Int, a)]
checkitoListOf i = itoListOf (ifolded % ix i)

checkPartsOf
  :: Traversable f
  => (f (Either a b), c)
  -> (f (Either a b), c)
checkPartsOf = partsOf (_1 % traversed % _Right) %~ reverse

checkSingular
  :: Traversable f
  => Either (f (a, Char)) b
  -> Either (f (a, Char)) b
checkSingular = singular (_Left % traversed % _2) .~ 'x'

checkFilteredBy
  :: Applicative f
  => ((Maybe i, b) -> f r)
  -> (Maybe i, b)
  -> f ()
checkFilteredBy = atraverseOf_ (filteredBy (_1 % _Just)) pure

checkUnsafeFilteredBy
  :: Applicative f
  => (i -> Either a1 (a, Maybe i) -> f (Either a1 (a, Maybe i)))
  -> Either a1 (a, Maybe i)
  -> f (Either a1 (a, Maybe i))
checkUnsafeFilteredBy = iatraverseOf (unsafeFilteredBy (_Right % _2 % _Just)) pure

checkAdjoin :: (a -> a) -> (Maybe a, Either a a, [a]) -> (Maybe a, Either a a, [a])
checkAdjoin = over (_1 % _Just `adjoin` _2 % chosen `adjoin` _3 % traversed)

checkIxAdjoin :: (Int -> a -> a) -> ((Int, a), [a], (Int, Maybe a)) -> ((Int, a), [a], (Int, Maybe a))
checkIxAdjoin = iover (_1 % itraversed `iadjoin` _2 % itraversed `iadjoin` _3 % itraversed % _Just)

checkNoAppendProof
  :: ( TraversableWithIndex i1 f1, TraversableWithIndex i2 f2
     , TraversableWithIndex i3 f3, TraversableWithIndex i4 f4
     , TraversableWithIndex i5 f5, TraversableWithIndex i6 f6
     , TraversableWithIndex i7 f7, TraversableWithIndex i8 f8
     ) => Optic A_Traversal
                '[i1, i2, i3, i4, i5, i6, i7, i8]
                (f1 (f2 (f3 (f4 (f5 (f6 (f7 (f8 a))))))))
                (f1 (f2 (f3 (f4 (f5 (f6 (f7 (f8 b))))))))
                a
                b
checkNoAppendProof
  = (((itraversed % itraversed) % itraversed) % itraversed)
  % (itraversed % (itraversed % (itraversed % itraversed)))
