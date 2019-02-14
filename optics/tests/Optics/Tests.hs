{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Inspection
import qualified Data.Map as M

import Optics
import Optics.Tests.Utils

-- | Composing a lens and a traversal yields a traversal
_comp1 :: Traversable t => Optic A_Traversal NoIx (t a, y) (t b, y) a b
_comp1 = _1 % traversed

-- | Composing two lenses yields a lens
_comp2 :: Optic A_Lens NoIx ((a, y), y1) ((b, y), y1) a b
_comp2 = _1 % _1

-- | Composing a getter and a lens yields a getter
_comp3 :: Optic A_Getter NoIx ((b, y), b1) ((b, y), b1) b b
_comp3 = to fst % _1

-- | Composing a prism and a lens yields a traversal
_comp4 :: Optic An_AffineTraversal NoIx (Either c (a, y)) (Either c (b, y)) a b
_comp4 = _Right % _1

-- | An iso can be used as a getter
_eg1 :: Int
_eg1 = view (iso (+ 1) (\ x -> x - 1)) 5

-- | A lens can be used as a getter
_eg2 :: (a, b) -> a
_eg2 = view _1

-- These don't typecheck, as one would expect:
--   to fst % mapped  -- Cannot compose a getter with a setter
--   toLens (to fst)  -- Cannot use a getter as a lens

-------------------------------------------------------------------------------
-- Inspection tests
-------------------------------------------------------------------------------

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

-- Eta expansion tests

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

-- Misc

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

simpleMapIx
  :: Ord k => k -> Either a (M.Map k (b, v)) -> Maybe v
simpleMapIx k = preview (_Right % ix k % _2)

mapIx
  :: (Foldable f, Foldable g, Ord k)
  => (f (Either a (g (M.Map k v))), b) -> k -> [v]
mapIx m k = toListOf (_1 % folded % _Right % folded % ix k) m

inspectionTests :: TestTree
inspectionTests = testGroup "inspection"
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
        assertSuccess $(inspectTest $ 'lhs04 === 'rhs04)
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
        assertSuccess $(inspectTest $ 'lhs06 === 'rhs06)
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
        -- Code is the same on GHC 8.0.2 modulo names of parameters.
        ghc80failure $(inspectTest $ 'lhs09 === 'rhs09)
    , testCase "optimized lhs09" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'lhs09)
    , testCase "optimized rhs09" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'rhs09)
    , testCase "itraverseOf_ itraversed = itraverseOf_ ifolded" $
        assertSuccess $(inspectTest $ 'lhs10 === 'rhs10)
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
        assertSuccess $(inspectTest $ 'lhs12 === 'rhs12)
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
        assertSuccess $(inspectTest $ 'lhs14 === 'rhs14)
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
        assertSuccess $(inspectTest $ 'lhs16 === 'rhs16)
    , testCase "optimized lhs16" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'lhs16)
    , testCase "optimized rhs16" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'rhs16)

    , testCase "toListOf folded = \\s -> toListOf folded s" $
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
        -- See the definition of itraverseOf_ for details.
        ghc82failure $(inspectTest $ 'eta6lhs === 'eta6rhs)
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

    , testCase "optimized failover" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'failoverCheck)
    , testCase "optimized failover'" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'failover'Check)
    , testCase "optimized ifailover" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'ifailoverCheck)
    , testCase "optimized ifailover'" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'ifailover'Check)
    , testCase "optimized sipleMapIx" $
        assertSuccess $(inspectTest $ 'simpleMapIx `hasNoTypeClassesExcept` [''Ord])
    , testCase "optimized mapIx" $
        assertSuccess $(inspectTest $ hasNoProfunctors 'mapIx)
    ]

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
compL03 f g s b = withAffineTraversal (atraversal f g) (\_ g' -> g') s b
compR03 _ g s b = g s b
compR03_ f g s b = case f s of
    Left t  -> t
    Right _ -> g s b

compL04, compR04 :: (s -> Either t a) -> (s -> b -> t) -> (s -> Either t a)
compL04 f g s = withAffineTraversal (atraversal f g) (\f' _ -> f') s
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
            assertSuccess $(inspectTest $ 'compL02 === 'compR02)
        ]
    , testGroup "AffineTraversal"
        -- this doesn't hold definitionally: we need law here
        [ testCase "withAffineTraversal (atraversal f g) (\\ _ g' -> g') /= g" $
             assertFailure' $(inspectTest $ 'compL03 === 'compR03)
        , testCase "withAffineTraversal (atraversal f g) (\\ _ g' -> g') = ..." $
             assertSuccess $(inspectTest $ 'compL03 ==- 'compR03_)
        , testCase "withAffineTraversal (atraversal f g) (\\ f' _ -> f') = f" $
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
