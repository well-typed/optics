{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Eta (etaTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Optics
import Optics.Tests.Utils

etaTests :: TestTree
etaTests = testGroup "Eta expansion"
  [ testCase "toListOf folded = \\s -> toListOf folded s" $
    assertSuccess $(inspectTest $ 'eta1lhs === 'eta1rhs)
  , testCase "optimized eta1lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta1lhs)
  , testCase "itoListOf ifolded = \\s -> itoListOf ifolded s" $
    assertSuccess $(inspectTest $ 'eta2lhs === 'eta2rhs)
  , testCase "optimized eta2lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta2lhs)
  , testCase "traverseOf traversed = \\f -> traverseOf traversed f" $
    assertSuccess $(inspectTest $ 'eta3lhs === 'eta3rhs)
  , testCase "optimized eta3lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta3lhs)
  , testCase "itraverseOf itraversed = \\f -> itraverseOf itraversed f" $
    assertSuccess $(inspectTest $ 'eta4lhs === 'eta4rhs)
  , testCase "optimized eta4lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta4lhs)
  , testCase "traverseOf_ folded = \\f -> traverseOf_ folded f" $
    assertSuccess $(inspectTest $ 'eta5lhs === 'eta5rhs)
  , testCase "optimized eta5lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta5lhs)
  , testCase "itraverseOf_ ifolded = \\f -> itraverseOf_ ifolded f" $
    -- The lhs has more lets which the rhs inlines.
    ghc82andGE90failure $(inspectTest $ 'eta6lhs === 'eta6rhs)
  , testCase "optimized eta6lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta6lhs)
  , testCase "over mapped = \\f -> over mapped f" $
    assertSuccess $(inspectTest $ 'eta7lhs === 'eta7rhs)
  , testCase "optimized eta7lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta7lhs)
  , testCase "over' mapped = \\f -> over' mapped f" $
    assertSuccess $(inspectTest $ 'eta8lhs === 'eta8rhs)
  , testCase "optimized eta8lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta8lhs)
  , testCase "iover imapped = \\f -> iover imapped f" $
    assertSuccess $(inspectTest $ 'eta9lhs === 'eta9rhs)
  , testCase "optimized eta9lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta9lhs)
  , testCase "iover' imapped = \\f -> iover' imapped f" $
    assertSuccess $(inspectTest $ 'eta10lhs === 'eta10rhs)
  , testCase "optimized eta10lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta10lhs)
  , testCase "iset imapped = \\f -> iset imapped f" $
    assertSuccess $(inspectTest $ 'eta11lhs === 'eta11rhs)
  , testCase "optimized eta11lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta11lhs)
  , testCase "iset' imapped = \\f -> iset' imapped f" $
    assertSuccess $(inspectTest $ 'eta12lhs === 'eta12rhs)
  , testCase "optimized eta12lhs" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'eta12lhs)
  ]

eta1lhs, eta1rhs :: Foldable f => f a -> [a]
eta1lhs   = toListOf folded
eta1rhs s = toListOf folded s

eta2lhs, eta2rhs :: FoldableWithIndex i f => f a -> [(i, a)]
eta2lhs   = itoListOf ifolded
eta2rhs s = itoListOf ifolded s

eta3lhs, eta3rhs
  :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
eta3lhs   = traverseOf traversed
eta3rhs f = traverseOf traversed f

eta4lhs, eta4rhs
  :: (Applicative f, TraversableWithIndex i t) => (i -> a -> f b) -> t a -> f (t b)
eta4lhs   = itraverseOf itraversed
eta4rhs f = itraverseOf itraversed f

eta5lhs, eta5rhs
  :: (Applicative f, Foldable t) => (a -> f r) -> t a -> f ()
eta5lhs   = traverseOf_ folded
eta5rhs f = traverseOf_ folded f

eta6lhs, eta6rhs
  :: (Applicative f, FoldableWithIndex i t) => (i -> a -> f r) -> t a -> f ()
eta6lhs   = itraverseOf_ ifolded
eta6rhs f = itraverseOf_ ifolded f

eta7lhs, eta7rhs
  :: Functor f => (a -> b) -> f a -> f b
eta7lhs   = over mapped
eta7rhs f = over mapped f

eta8lhs, eta8rhs
  :: Functor f => (a -> b) -> f a -> f b
eta8lhs   = over' mapped
eta8rhs f = over' mapped f

eta9lhs, eta9rhs
  :: FunctorWithIndex i f => (i -> a -> b) -> f a -> f b
eta9lhs   = iover imapped
eta9rhs f = iover imapped f

eta10lhs, eta10rhs
  :: FunctorWithIndex i f => (i -> a -> b) -> f a -> f b
eta10lhs   = iover' imapped
eta10rhs f = iover' imapped f

eta11lhs, eta11rhs
  :: (FunctorWithIndex i f, FunctorWithIndex j g)
  => ((i, j) -> b) -> f (g a) -> f (g b)
eta11lhs   = iset (imapped <%> imapped)
eta11rhs f = iset (imapped <%> imapped) f

eta12lhs, eta12rhs
  :: (FunctorWithIndex i f, FunctorWithIndex j g)
  => ((i, j) -> b) -> f (g a) -> f (g b)
eta12lhs   = iset' (imapped <%> imapped)
eta12rhs f = iset' (imapped <%> imapped) f

-- workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/26436
_unused :: ()
_unused = const ()
  [ 'eta1lhs, 'eta1rhs
  , 'eta2lhs, 'eta2rhs
  , 'eta3lhs, 'eta3rhs
  , 'eta4lhs, 'eta4rhs
  , 'eta5lhs, 'eta5rhs
  , 'eta6lhs, 'eta6rhs
  , 'eta7lhs, 'eta7rhs
  , 'eta8lhs, 'eta8rhs
  , 'eta9lhs, 'eta9rhs
  , 'eta10lhs, 'eta10rhs
  , 'eta11lhs, 'eta11rhs
  , 'eta12lhs, 'eta12rhs
  ]
