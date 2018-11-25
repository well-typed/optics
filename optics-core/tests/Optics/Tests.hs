-- | At the moment, merely compiling this module is all the testing we do.
module Main where

import Data.Either.Optics
import Data.Tuple.Optics
import Optics

-- | Composing a lens and a traversal yields a traversal
comp1 :: Traversable t => Optic A_Traversal i i (t a, y) (t b, y) a b
comp1 = _1 % traversed

-- | Composing two lenses yields a lens
comp2 :: Optic A_Lens i i ((a, y), y1) ((b, y), y1) a b
comp2 = _1 % _1

-- | Composing a getter and a lens yields a getter
comp3 :: Optic A_Getter i i ((b, y), b1) ((b, y), b1) b b
comp3 = to fst % _1

-- | Composing a prism and a lens yields a traversal
comp4 :: Optic An_AffineTraversal i i (Either c (a, y)) (Either c (b, y)) a b
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

main :: IO ()
main = return ()
