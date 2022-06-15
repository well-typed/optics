{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin -dsuppress-all #-}
module Optics.Tests.Core (coreTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Optics
import Optics.Tests.Utils

coreTests :: TestTree
coreTests = testGroup "Core"
  [ testCase "traverseOf traversed = traverse" $
    assertSuccess $(inspectTest $ 'lhs01 === 'rhs01)
  , testCase "optimized lhs01a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs01a)
  , testCase "traverseOf (traversed % traversed) = traverse . traverse" $
    assertSuccess $(inspectTest $ 'lhs02 === 'rhs02)
  , testCase "optimized lhs02a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs02a)
  , testCase "traverseOf (traversed % traversed) = traverseOf (itraversed % itraversed)" $
    assertSuccess $(inspectTest $ 'lhs03 === 'rhs03)
  , testCase "optimized lhs03" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs03)
  , testCase "optimized rhs03" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs03)
  , testCase "traverseOf_ (folded % folded) = traverseOf_ (ifolded % ifolded)" $
    assertSuccess $(inspectTest $ 'lhs04 ==- 'rhs04)
  , testCase "optimized lhs04" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs04)
  , testCase "optimized rhs04" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs04)
  , testCase "over (noIx (imapped % imapped)) = over (mapped % mapped)" $
    assertSuccess $(inspectTest $ 'lhs05 === 'rhs05)
  , testCase "over (imapped % imapped) = over (mapped % mapped)" $
    assertSuccess $(inspectTest $ 'lhs05b === 'rhs05)
  , testCase "optimized lhs05" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs05)
  , testCase "optimized rhs05" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs05)
  , testCase "traverseOf_ (_Left % itraversed % _1 % ifolded) = traverseOf_ ..." $
    -- GHC >= 8.6 gives different structure of let bindings.
    ghcGE86failure $(inspectTest $ 'lhs06 === 'rhs06)
  , testCase "optimized lhs06" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs06)
  , testCase "optimized rhs06" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs06)
  , testCase "itraverseOf (itraversed %> itraversed) = itraverseOf (traversed % itraversed)" $
    assertSuccess $(inspectTest $ 'lhs07 === 'rhs07)
  , testCase "optimized lhs07a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs07a)
  , testCase "optimized rhs07a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs07a)
  , testCase "itraverseOf_ (ifolded %> ifolded) ==- itraverseOf (folded % ifolded)" $
    -- Same code modulo coercions.
    assertSuccess $(inspectTest $ 'lhs08 ==- 'rhs08)
  , testCase "optimized lhs08a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs08a)
  , testCase "optimized rhs08a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs08a)
  , testCase "iover (imapped <% imapped) = iover (imapped % mapped)" $
    -- GHC 9.* applies a worker-wrapper transformation to the RHS.
    ghcGE90failure $(inspectTest $ 'lhs09 === 'rhs09)
  , testCase "optimized lhs09" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs09)
  , testCase "optimized rhs09" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs09)
  , testCase "itraverseOf_ itraversed = itraverseOf_ ifolded" $
    -- GHC 8.2, 8.6 to 8.10 and 9.2 to 9.4 give a different structure of let
    -- bindings.
    assertSuccess $(inspectTest $ 'lhs10 ==~ 'rhs10)
  , testCase "optimized lhs10a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs10a)
  , testCase "optimized rhs10a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs10a)
  , testCase "iover (itraversed..itraversed) = iover (imapped..imapped)" $
    assertSuccess $(inspectTest $ 'lhs11 === 'rhs11)
  , testCase "optimized lhs11" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs11)
  , testCase "optimized rhs11" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs11)
  , testCase "traverseOf_ traversed = traverseOf_ folded" $
    -- GHC 8.6 to 8.10 give a different structure of let bindings.
    assertSuccess $(inspectTest $ 'lhs12 ==~ 'rhs12)
  , testCase "optimized lhs12a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs12a)
  , testCase "optimized rhs12a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs12a)
  , testCase "over (traversed..) = over (mapped..)" $
    assertSuccess $(inspectTest $ 'lhs13 === 'rhs13)
  , testCase "optimized lhs13" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs13)
  , testCase "optimized rhs13" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs13)
  , testCase "traverseOf_ itraversed = traverseOf_ folded" $
    -- GHC 8.6 to 8.10 give a different structure of let bindings
    -- GHC 9.2 to 9.4 have very different structure
    ghcGE92failure $(inspectTest $ 'lhs14 ==~ 'rhs14)
  , testCase "optimized lhs14a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs14a)
  , testCase "optimized rhs14a" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs14a)
  , testCase "over (itraversed..) = over (mapped..)" $
    assertSuccess $(inspectTest $ 'lhs15 === 'rhs15)
  , testCase "optimized lhs15" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs15)
  , testCase "optimized rhs15" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs15)
  , testCase "iset (itraversed..) = iset (imapped..)" $
    -- GHC 8.10 has an intermediate let in the RHS.
    ghc810failure $(inspectTest $ 'lhs16 === 'rhs16)
  , testCase "optimized lhs16" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs16)
  , testCase "optimized rhs16" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs16)
  , testCase "iset (_1 % itraversed) = iset (_1 % imapped)" $
    -- GHC 8.10 has an intermediate let in the LHS.
    ghc810failure $(inspectTest $ 'lhs17 === 'rhs17)
  , testCase "optimized lhs17" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs17)
  , testCase "optimized rhs17" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs17)
  , testCase "iset (each %> itraversed) = iset (each %> imapped)" $
    assertSuccess $(inspectTest $ 'lhs18 === 'rhs18)
  , testCase "optimized lhs18" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'lhs18)
  , testCase "optimized rhs18" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'rhs18)
  , testCase "optimized failover" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'failoverCheck)
  , testCase "optimized failover'" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'failover'Check)
  , testCase "optimized ifailover" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'ifailoverCheck)
  , testCase "optimized ifailover'" $
    assertSuccess $(inspectTest $ hasNoProfunctors 'ifailover'Check)
  ]

-- Sometimes we need to eta expand, as without it pretty much equivalent code is
-- produced, but somewhat rearranged. Expanding allows us to get rid of these
-- differences to satisfy the check. However, when we do that we also need to
-- check whether the form that is not eta-expanded optimizes away internal
-- representation correctly.

lhs01, rhs01, lhs01a
  :: (Applicative f, Traversable t)
  => (a -> f b) -> t a -> f (t b)
lhs01 f = traverseOf traversed f
rhs01 f = traverse f
lhs01a = traverseOf traversed

lhs02, rhs02, lhs02a
  :: (Applicative f, Traversable t, Traversable s)
  => (a -> f b) -> t (s a) -> f (t (s b))
lhs02 f = traverseOf (traversed % traversed) f
rhs02 f = traverse . traverse $ f
lhs02a = traverseOf (traversed % traversed)

lhs03, rhs03
  :: (Applicative f, TraversableWithIndex i t, TraversableWithIndex j s)
  => (a -> f b) -> t (s a) -> f (t (s b))
lhs03 = traverseOf (traversed % traversed)
rhs03 = traverseOf (itraversed % itraversed)

lhs04, rhs04
  :: (Applicative f, FoldableWithIndex i t, FoldableWithIndex j s)
  => (a -> f r) -> t (s a) -> f ()
lhs04 = traverseOf_ (folded % folded)
rhs04 = traverseOf_ (ifolded % ifolded)

lhs05, lhs05b, rhs05
  :: (FunctorWithIndex i f, FunctorWithIndex j g) => (a -> b) -> f (g a) -> f (g b)
lhs05  = over (noIx (imapped % imapped))
lhs05b = over (imapped % imapped)
rhs05  = over (mapped % mapped)

lhs06, rhs06
  :: (Applicative f, TraversableWithIndex i t, FoldableWithIndex j f)
  => (a -> f r)
  -> (Either (t (f a, c)) b)
  -> f ()
lhs06 = traverseOf_ (_Left % ifolded % _1 % ifolded)
rhs06 = traverseOf_ (_Left % folded % _1 % folded)

lhs07, rhs07, lhs07a, rhs07a
  :: (Applicative f, TraversableWithIndex i t, TraversableWithIndex j s)
  => (j -> a -> f b)
  -> t (s a)
  -> f (t (s b))
lhs07 f = itraverseOf (itraversed %> itraversed) f
rhs07 f = itraverseOf (traversed % itraversed) f
lhs07a = itraverseOf (itraversed %> itraversed)
rhs07a = itraverseOf (traversed % itraversed)

lhs08, rhs08, lhs08a, rhs08a
  :: (Applicative f, FoldableWithIndex i t, FoldableWithIndex j s)
  => (j -> a -> f ())
  -> t (s a)
  -> f ()
lhs08 f = itraverseOf_ (ifolded %> ifolded) f
rhs08 f = itraverseOf_ (folded % ifolded) f
lhs08a = itraverseOf_ (ifolded %> ifolded)
rhs08a = itraverseOf_ (folded % ifolded)

lhs09, rhs09
  :: (FunctorWithIndex i t, FunctorWithIndex j s)
  => (i -> a -> b)
  -> t (s a)
  -> t (s b)
lhs09 = iover (imapped <% imapped)
rhs09 = iover (imapped % mapped)

-- Rewrite rule "itraversed__ -> ifolded__"
lhs10, rhs10, lhs10a, rhs10a
  :: (Applicative f, TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> a -> f r)
  -> s (Either (t a) b)
  -> f ()
lhs10 f s = itraverseOf_ (icompose (,) $ itraversed % _Left % itraversed) f s
rhs10 f s = itraverseOf_ (icompose (,) $ ifolded % _Left % ifolded) f s
lhs10a = itraverseOf_ (icompose (,) $ itraversed % _Left % itraversed)
rhs10a = itraverseOf_ (icompose (,) $ ifolded % _Left % ifolded)

-- Rewrite rule "itraversed__ -> imapped__"
lhs11, rhs11
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> a -> b)
  -> s (Either c (t a))
  -> s (Either c (t b))
lhs11 = iover (icompose (,) $ itraversed % _Right % itraversed)
rhs11 = iover (icompose (,) $ imapped % _Right % imapped)

-- Rewrite rule "traversed__ -> folded__"
lhs12, rhs12, lhs12a, rhs12a
  :: (Applicative f, Traversable s, Traversable t)
  => (a -> f r)
  -> s (Either (t a) b)
  -> f ()
lhs12 f = traverseOf_ (traversed % _Left % traversed) f
rhs12 f = traverseOf_ (folded % _Left % folded) f
lhs12a = traverseOf_ (traversed % _Left % traversed)
rhs12a = traverseOf_ (folded % _Left % folded)

-- Rewrite rule "traversed__ -> mapped__"
lhs13, rhs13
  :: Traversable s
  => (a -> b)
  -> s (Either c a)
  -> s (Either c b)
lhs13 = over (traversed % _Right)
rhs13 = over (mapped % _Right)

-- Rewrite rule "itraversed__ -> folded__"
lhs14, rhs14, lhs14a, rhs14a
  :: (Applicative f, TraversableWithIndex i s, Traversable t)
  => (a -> f r)
  -> s (Either (t a) b)
  -> f ()
lhs14 f = traverseOf_ (itraversed % _Left % traversed) f
rhs14 f = traverseOf_ (folded % _Left % folded) f
lhs14a = traverseOf_ (itraversed % _Left % traversed)
rhs14a = traverseOf_ (folded % _Left % folded)

-- Rewrite rule "itraversed__ -> mapped__"
lhs15, rhs15
  :: TraversableWithIndex i s
  => (a -> b)
  -> s (Either c a)
  -> s (Either c b)
lhs15 = over (itraversed % _Right)
rhs15 = over (mapped % _Right)

lhs16, rhs16
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> b)
  -> s (Either c (t a))
  -> s (Either c (t b))
lhs16 = iset (icompose (,) $ itraversed % _Right % itraversed)
rhs16 = iset (icompose (,) $ imapped % _Right % imapped)

lhs17, rhs17
  :: (TraversableWithIndex i s)
  => (i -> b)
  -> (s a, r)
  -> (s b, r)
lhs17 = iset (_1 % itraversed)
rhs17 = iset (_1 % imapped)

lhs18, rhs18
  :: (TraversableWithIndex i s)
  => (i -> a)
  -> [s a]
  -> [s a]
lhs18 = iset (each %> itraversed)
rhs18 = iset (each %> imapped)

failoverCheck
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => (a -> b)
  -> s (Either c (t a))
  -> Maybe (s (Either c (t b)))
failoverCheck = failover (traversed % _Right % traversed)

failover'Check
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => (a -> b)
  -> s (Either c (t a))
  -> Maybe (s (Either c (t b)))
failover'Check = failover' (traversed % _Right % traversed)

ifailoverCheck
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> a -> b)
  -> s (Either c (t a))
  -> Maybe (s (Either c (t b)))
ifailoverCheck = ifailover (icompose (,) $ itraversed % _Right % itraversed)

ifailover'Check
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> a -> b)
  -> s (Either c (t a))
  -> Maybe (s (Either c (t b)))
ifailover'Check = ifailover' (icompose (,) $ itraversed % _Right % itraversed)
