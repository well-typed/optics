{-# LANGUAGE DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Remnants that have not yet been moved to other modules.
--
module Optics.Internal where

import Optics.Internal.Fold
import Optics.Internal.Getter
import Optics.Internal.Iso
import Optics.Internal.Lens
import Optics.Internal.Optic
import Optics.Internal.Traversal
import Optics.Internal.Prism
import Optics.Internal.Setter
import Optics.Internal.Subtyping ()



mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap


traversalOf :: forall k s t a b . Is k A_Traversal => Optic k s t a b -> Optic_ A_Traversal s t a b
traversalOf = getOptic . toTraversal


_1 :: Lens (a,y) (b,y) a b
_1 = lens fst (\ (_,y) x -> (x,y))


_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right (either (Left . Left) Right)






-- | A constraint that can never be satisfied (accompanied by a
-- helpful witness to anything you like).
class Absurd where
  absurd :: a

-- | In order to get nice error messages, we complete the type lattice
-- with a universal supertype, whose constraints can never be
-- satisfied.
data Bogus
type instance Constraints Bogus p = Absurd
instance Is k Bogus where
  implies = absurd

-- | This typeclass has no instances, and is used so that we get
-- suitable unsolved constraint errors when attempting to compose
-- flavours that do not have a (non-'Bogus') common supertype.
--
-- For example, if you try to compose a fold with a setter then you
-- will get an error
--
-- > Could not deduce (CanCompose A_Fold A_Setter)
class Absurd => CanCompose k l

instance CanCompose A_Fold   A_Setter => Join A_Fold   A_Setter Bogus
instance CanCompose A_Setter A_Fold   => Join A_Setter A_Fold   Bogus
instance CanCompose A_Setter A_Getter => Join A_Setter A_Getter Bogus
instance CanCompose A_Getter A_Setter => Join A_Getter A_Setter Bogus


-- | Composing a lens and a traversal yields a traversal
comp1 :: Traversable t => Optic A_Traversal (t a, y) (t b, y) a b
comp1 = _1 % _traverse

-- | Composing two lenses yields a lens
comp2 :: Optic A_Lens ((a, y), y1) ((b, y), y1) a b
comp2 = _1 % _1

-- | Composing a getter and a lens yields a getter
comp3 :: Optic A_Getter ((b, y), b1) ((b, y), b1) b b
comp3 = to fst % _1

-- | Composing a prism and a lens yields a traversal
comp4 :: Optic A_Traversal (Either c (a, y)) (Either c (b, y)) a b
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
