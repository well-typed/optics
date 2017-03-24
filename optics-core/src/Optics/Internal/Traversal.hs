{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Optics.Internal.Traversal where

-- import Data.Functor.Identity (Identity(..))
-- import Data.Traversable

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a traversal.
data A_Traversal

-- | Constraints corresponding to a traversal.
type instance Constraints A_Traversal p = Wandering p

-- | Type synonym for a type-modifying traversal.
type Traversal s t a b = Optic A_Traversal s t a b

-- | Type synonym for a type-preserving traversal.
type Traversal' s a = Optic' A_Traversal s a

-- | Explicitly cast an optic to a traversal.
toTraversal :: Is k A_Traversal => Optic k s t a b -> Traversal s t a b
toTraversal = sub
{-# INLINE toTraversal #-}

{-
-- | Internal representation for converting between traversal
-- representations.
--
-- In particular,
--
-- s -> Bazaar a b t
--
-- is isomorphic to
--
-- Traversal s t a b
--
newtype Bazaar a b t =
  Bazaar { runBazaar :: forall f . Applicative f => (a -> f b) -> f t }

idBazaar :: a -> Bazaar a b b
idBazaar a = Bazaar ($ a)
{-# INLINE idBazaar #-}

instance Functor (Bazaar a b) where
  fmap st (Bazaar afbfs) = Bazaar (fmap st . afbfs)
  {-# INLINE fmap #-}

instance Applicative (Bazaar a b) where
  pure t = Bazaar (const (pure t))
  {-# INLINE pure #-}
  Bazaar afbfst <*> Bazaar afbfs =
    Bazaar (\ afb -> afbfst afb <*> afbfs afb)
  {-# INLINE (<*>) #-}

newtype TraversableBazaar b t a =
  TraversableBazaar { runTraversableBazaar :: Bazaar a b t }

instance Functor (TraversableBazaar b t) where
  fmap = fmapDefault

instance Foldable (TraversableBazaar b t) where
  foldMap = foldMapDefault

instance Traversable (TraversableBazaar b t) where
  traverse afc (TraversableBazaar (Bazaar afbft)) =
    undefined

-- have:
-- - forall f . Applicative f => (a -> f b) -> f t
-- - a -> f c
--
-- want:
--
-- - forall g . Applicative g => (a -> g c) -> g t
--
-- instantiate f to (g . Const c):

-- | Create a traversal.
vlTraversal :: (forall f . Applicative f => (a -> f b) -> (s -> f t)) -> Traversal s t a b
vlTraversal o =
  mkTraversal (dimap (TraversableBazaar . o idBazaar) (runIdentity . ($ Identity) . runBazaar . runTraversableBazaar) . wander)
-}

-- | Create a traversal.
mkTraversal :: Optic_ A_Traversal s t a b -> Traversal s t a b
mkTraversal = Optic
{-# INLINE mkTraversal #-}

-- | Traversal via the 'Traversal' class.
--
-- TODO: This function is not necessary in 'lens', one can simply
-- use 'traverse'. The name here is preliminary.
--
_traverse :: Traversable t => Traversal (t a) (t b) a b
_traverse = mkTraversal wander
{-# INLINE _traverse #-}
