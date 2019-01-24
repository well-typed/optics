{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection

import Data.Either.Optics
import Data.Tuple.Optics
import Optics

-- | Composing a lens and a traversal yields a traversal
comp1 :: Traversable t => Optic A_Traversal NoIx (t a, y) (t b, y) a b
comp1 = _1 % traversed

-- | Composing two lenses yields a lens
comp2 :: Optic A_Lens NoIx ((a, y), y1) ((b, y), y1) a b
comp2 = _1 % _1

-- | Composing a getter and a lens yields a getter
comp3 :: Optic A_Getter NoIx ((b, y), b1) ((b, y), b1) b b
comp3 = to fst % _1

-- | Composing a prism and a lens yields a traversal
comp4 :: Optic An_AffineTraversal NoIx (Either c (a, y)) (Either c (b, y)) a b
comp4 = _Right % _1

-- | An iso can be used as a getter
eg1 :: Int
eg1 = view (iso (+ 1) (\ x -> x - 1)) 5

-- | A lens can be used as a getter
eg2 :: (a, b) -> a
eg2 = view _1

-- These don't typecheck, as one would expect:
--   to fst % mapped  -- Cannot compose a getter with a setter
--   toLens (to fst)  -- Cannot use a getter as a lens

-------------------------------------------------------------------------------
-- Inspection tests
-------------------------------------------------------------------------------

-- eta-expansion is required
lhs01, rhs01
  :: (Applicative f, Traversable t)
  => (a -> f b) -> t a -> f (t b)
lhs01 f = traverseOf traversed f
rhs01 f = traverse f

lhs02, rhs02
  :: (Applicative f, Traversable t, Traversable s)
  => (a -> f b) -> t (s a) -> f (t (s b))
lhs02 f = traverseOf (traversed % traversed) f
rhs02 f = traverse . traverse $ f

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

lhs07, rhs07
  :: (Applicative f, TraversableWithIndex i t, TraversableWithIndex j s)
  => (j -> a -> f b)
  -> t (s a)
  -> f (t (s b))
lhs07 = itraverseOf (itraversed %> itraversed)
rhs07 = itraverseOf (traversed % itraversed)

-- This doesn't quite work, i.e. it seems to generate the same code, but modulo
-- coercions.
lhs08, rhs08
  :: (Applicative f, FoldableWithIndex i t, FoldableWithIndex j s)
  => (j -> a -> f ())
  -> t (s a)
  -> f ()
lhs08 = itraverseOf_ (ifolded %> ifolded)
rhs08 = itraverseOf_ (folded % ifolded)

lhs09, rhs09
  :: (FunctorWithIndex i t, FunctorWithIndex j s)
  => (i -> a -> b)
  -> t (s a)
  -> t (s b)
lhs09 = iover (imapped <% imapped)
rhs09 = iover (imapped % mapped)

-- Rewrite rule "itraversed__ -> ifolded__" - doesn't quite work similarly to
-- 08, seems to generate the same code, but modulo coercions.
lhs10, rhs10
  :: (Applicative f, TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> a -> f r)
  -> s (Either (t a) b)
  -> f ()
lhs10 = itraverseOf_ (icompose (,) $ itraversed % _Left % itraversed)
rhs10 = itraverseOf_ (icompose (,) $ ifolded % _Left % ifolded)

-- Rewrite rule "itraversed__ -> imapped__"
lhs11, rhs11
  :: (TraversableWithIndex i s, TraversableWithIndex j t)
  => ((i, j) -> a -> b)
  -> s (Either c (t a))
  -> s (Either c (t b))
lhs11 = iover (icompose (,) $ itraversed % _Right % itraversed)
rhs11 = iover (icompose (,) $ imapped % _Right % imapped)

inspectionTests :: TestTree
inspectionTests = testGroup "inspection"
    [ testCase "traverseOf traversed = traverse" $
        assertSuccess $(inspectTest $ 'lhs01 === 'rhs01)
    , testCase "traverseOf (traversed % traversed) = \\
                \traverse . traverse" $
        assertSuccess $(inspectTest $ 'lhs02 === 'rhs02)
    , testCase "traverseOf (traversed % traversed) = \\
               \traverseOf (itraversed % itraversed)" $
        assertSuccess $(inspectTest $ 'lhs03 === 'rhs03)
    , testCase "traverseOf_ (folded % folded) = \\
               \traverseOf_ (ifolded % ifolded)" $
        assertSuccess $(inspectTest $ 'lhs04 === 'rhs04)
    , testCase "over (noIx (imapped % imapped)) = \\
               \over (mapped % mapped)" $
        assertSuccess $(inspectTest $ 'lhs05 === 'rhs05)
    , testCase "over (imapped % imapped) = \\
               \over (mapped % mapped)" $
        assertSuccess $(inspectTest $ 'lhs05b === 'rhs05)
    , testCase "traverseOf_ (_Left % itraversed % _1 % ifolded) = \\
               \traverseOf_ ..." $
        assertSuccess $(inspectTest $ 'lhs06 === 'rhs06)
    , testCase "itraverseOf (itraversed %> itraversed) = \\
               \itraverseOf (traversed % itraversed)" $
        assertSuccess $(inspectTest $ 'lhs07 === 'rhs07)
    , testCase "itraverseOf_ (ifolded %> ifolded) =/= \\
               \itraverseOf (folded % ifolded)" $
        assertFailure' $(inspectTest $ 'lhs08 === 'rhs08)
    , testCase "iover (imapped <% imapped) = \\
               \iover (imapped % mapped)" $
        assertSuccess $(inspectTest $ 'lhs09 === 'rhs09)
#if __GLASGOW_HASKELL__ >= 802
    , testCase "itraverseOf_ (itraversed..itraversed) =/= \\
               \itraverseOf_ (ifolded..ifolded)" $
        assertFailure' $(inspectTest $ 'lhs10 === 'rhs10)
#else
    , testCase "itraverseOf_ (itraversed..itraversed) = \\
               \itraverseOf_ (ifolded..ifolded)" $
        assertSuccess $(inspectTest $ 'lhs10 === 'rhs10)
#endif
    , testCase "iover (itraversed..itraversed) = \\
               \iover (imapped..imapped)" $
        assertSuccess $(inspectTest $ 'lhs11 === 'rhs11)
    ]

assertSuccess :: Result -> IO ()
assertSuccess (Success _)   = return ()
assertSuccess (Failure err) = assertFailure err

assertFailure' :: Result -> IO ()
assertFailure' (Success err) = assertFailure err
assertFailure' (Failure _)   = return ()

-------------------------------------------------------------------------------
-- Computation tests
-------------------------------------------------------------------------------

compL01, compR01 :: (s -> a) -> (s -> a -> s) -> (s -> a)
compL01 f g s = view (lens f g) s
compR01 f _ s = f s

compL02, compR02 :: (s -> a) -> (s -> b -> t) -> (s -> b -> t)
compL02 f g s b = set (lens f g) b s
compR02 _ g s b = g s b

compL03, compR03, compR03_ :: (s -> Either t a) -> (s -> b -> t) -> (s -> b -> t)
compL03 f g s b = withAffineTraversal (atraversal f g) (\h _ -> h) s b
compR03 _ g s b = g s b
compR03_ f g s b = case f s of
    Left t  -> t
    Right _ -> g s b

compL04, compR04 :: (s -> Either t a) -> (s -> b -> t) -> (s -> Either t a)
compL04 f g s = withAffineTraversal (atraversal f g) (\_ h -> h) s
compR04 f _ s = f s

compL05, compR05 :: ((a -> b) -> s -> t) -> ((a -> b) -> s -> t)
compL05 f ab s = over (sets f) ab s
compR05 f ab s = f ab s

computationTests :: TestTree
computationTests = testGroup "computation"
    [ testGroup "Lens"
        [ testCase "view (lens f g) = f" $
            assertSuccess $(inspectTest $ 'compL01 === 'compR01)
        , testCase "set (lens f g) = g" $
            assertSuccess $(inspectTest $ 'compL01 === 'compR01)
        ]
    , testGroup "AffineTraversal"
        -- this doesn't hold definitionally: we need law here
        [ testCase "withAffineTraversal (atraversal f g) (\\ h _ -> h) /= g" $
             assertFailure' $(inspectTest $ 'compL03 === 'compR03)
        , testCase "withAffineTraversal (atraversal f g) (\\ h _ -> h) = ..." $
             assertSuccess $(inspectTest $ 'compL03 === 'compR03_)
        , testCase "withAffineTraversal (atraversal f g) (\\ _ h -> h) = f" $
            assertSuccess $(inspectTest $ 'compL04 === 'compR04)
        ]

    , testGroup "Setter"
        [ testCase "over (sets f) = f" $
            assertSuccess $(inspectTest $ 'compL05 === 'compR05)
        ]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ inspectionTests
    , computationTests
    ]
