module Optics.Plated
  (
  -- * Uniplate
    Plated(..)
  -- * Uniplate combinators
  -- ** Children
  , children
  -- ** Rewriting
  , rewrite
  , rewriteOf
  , rewriteOn
  , rewriteOnOf
  , rewriteM
  , rewriteMOf
  , rewriteMOn
  , rewriteMOnOf
  -- ** Universe
  , universe
  , universeOf
  , universeOn
  , universeOnOf
  , cosmos
  , cosmosOf
  , cosmosOn
  , cosmosOnOf
  -- ** Transformation
  , transform
  , transformOf
  , transformOn
  , transformOnOf
  , transformM
  , transformMOf
  , transformMOn
  , transformMOnOf
  -- ** Paramorphisms
  , para
  , paraOf

  -- * Parts
  , parts
  ) where

import Data.Tree

import Optics.Fold
import Optics.Generic
import Optics.Lens
import Optics.Optic
import Optics.Setter
import Optics.Traversal

class Plated a where
  -- | 'Traversal' of the immediate children of this structure.
  plate :: Traversal' a a
  default plate :: GPlate a a => Traversal' a a
  plate = gplate
  {-# INLINE plate #-}

instance Plated [a] where
  plate = traversalVL $ \f -> \case
    x : xs -> (x :) <$> f xs
    []     -> pure []

instance Plated (Tree a) where
  plate = traversalVL $ \f (Node a as) -> Node a <$> traverse f as

-------------------------------------------------------------------------------
-- Children
-------------------------------------------------------------------------------

-- | Extract the immediate descendants of a 'Plated' container.
--
-- @
-- 'children' ≡ 'toListOf' 'plate'
-- @
children :: Plated a => a -> [a]
children = toListOf plate
{-# INLINE children #-}

-------------------------------------------------------------------------------
-- Rewriting
-------------------------------------------------------------------------------

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @
-- propRewrite r x = 'all' ('Data.Just.isNothing' '.' r) ('universe' ('rewrite' r x))
-- @
--
-- Usually 'transform' is more appropriate, but 'rewrite' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\\a -> f a '<|>' g a@ which performs both rewrites until a fixed
-- point.
rewrite :: Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate
{-# INLINE rewrite #-}

-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- @
-- propRewriteOf l r x = 'all' ('Data.Just.isNothing' '.' r) ('universeOf' l ('rewriteOf' l r x))
-- @
--
-- Usually 'transformOf' is more appropriate, but 'rewriteOf' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @\\a -> f a '<|>' g a@ which performs both rewrites until a fixed
-- point.
rewriteOf :: Is k A_Setter => Optic k is a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go
  where
    go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}

-- | Rewrite recursively over part of a larger structure.
rewriteOn :: (Is k A_Setter, Plated a) => Optic k is s t a a -> (a -> Maybe a) -> s -> t
rewriteOn b = over b . rewrite
{-# INLINE rewriteOn #-}

-- | Rewrite recursively over part of a larger structure using a specified 'Setter'.
rewriteOnOf
  :: (Is k1 A_Setter, Is k2 A_Setter)
  => Optic k1 is1 s t a b
  -> Optic k2 is2 a b a b
  -> (b -> Maybe a) -> s -> t
rewriteOnOf b l = over b . rewriteOf l
{-# INLINE rewriteOnOf #-}

-- | Rewrite by applying a monadic rule everywhere you can. Ensures that the
-- rule cannot be applied anywhere in the result.
rewriteM :: (Plated a, Monad m) => (a -> m (Maybe a)) -> a -> m a
rewriteM = rewriteMOf plate
{-# INLINE rewriteM #-}

-- | Rewrite by applying a monadic rule everywhere you recursing with a
-- user-specified 'Traversal'.
--
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteMOf
  :: (Is k A_Traversal, Monad m)
  => Optic k is a b a b
  -> (b -> m (Maybe a)) -> a -> m b
rewriteMOf l f = go
  where
    go = transformMOf l (\x -> f x >>= maybe (return x) go)
{-# INLINE rewriteMOf #-}

-- | Rewrite by applying a monadic rule everywhere inside of a structure located by a user-specified 'Traversal'.
-- Ensures that the rule cannot be applied anywhere in the result.
rewriteMOn
  :: (Is k A_Traversal, Plated a, Monad m)
  => Optic k is s t a a
  -> (a -> m (Maybe a)) -> s -> m t
rewriteMOn b = traverseOf b . rewriteM
{-# INLINE rewriteMOn #-}

-- | Rewrite by applying a monadic rule everywhere inside of a structure located
-- by a user-specified 'Traversal', using a user-specified 'Traversal' for
-- recursion. Ensures that the rule cannot be applied anywhere in the result.
rewriteMOnOf
  :: (Is k1 A_Traversal, Is k2 A_Traversal, Monad m)
  => Optic k1 is1 s t a b
  -> Optic k2 is2 a b a b
  -> (b -> m (Maybe a)) -> s -> m t
rewriteMOnOf b l = traverseOf b . rewriteMOf l
{-# INLINE rewriteMOnOf #-}

-------------------------------------------------------------------------------
-- Universe
-------------------------------------------------------------------------------

-- | Retrieve all of the transitive descendants of a 'Plated' container,
-- including itself.
universe :: Plated a => a -> [a]
universe = universeOf plate
{-# INLINE universe #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of
-- the transitive descendants of a node, including itself.
universeOf :: Is k A_Fold => Optic' k is a a -> a -> [a]
universeOf l = go
  where
    go a = a : foldMapOf l go a
{-# INLINE universeOf #-}

-- | Given a 'Fold' that knows how to find 'Plated' parts of a container
-- retrieve them and all of their descendants, recursively.
universeOn :: (Is k A_Fold, Plated a) => Optic' k is s a -> s -> [a]
universeOn b = universeOnOf b plate
{-# INLINE universeOn #-}

-- | Given a 'Fold' that knows how to locate immediate children, retrieve all of
-- the transitive descendants of a node, including itself that lie in a region
-- indicated by another 'Fold'.
--
-- @
-- 'toListOf' l ≡ 'universeOnOf' l 'ignored'
-- @
universeOnOf
  :: (Is k1 A_Fold, Is k2 A_Fold)
  => Optic' k1 is1 s a
  -> Optic' k2 is2 a a
  -> s -> [a]
universeOnOf b = foldMapOf b . universeOf
{-# INLINE universeOnOf #-}

-- | Fold over all transitive descendants of a 'Plated' container, including
-- itself.
cosmos :: Plated a => Fold a a
cosmos = cosmosOf plate
{-# INLINE cosmos #-}

-- | Given a 'Fold' that knows how to locate immediate children, fold all of the
-- transitive descendants of a node, including itself.
cosmosOf :: forall k is a. Is k A_Fold => Optic' k is a a -> Fold a a
cosmosOf d = foldVL go
  where
    go :: Applicative f => (a -> f ()) -> a -> f ()
    go f a = f a *> traverseOf_ d (go f) a
{-# INLINE cosmosOf #-}

-- | Given a 'Fold' that knows how to find 'Plated' parts of a container fold
-- them and all of their descendants, recursively.
cosmosOn :: (Is k A_Fold, Plated a) => Optic' k is s a -> Optic' A_Fold is s a
cosmosOn o = cosmosOnOf o plate
{-# INLINE cosmosOn #-}

-- | Given a 'Fold' that knows how to locate immediate children, fold all of the
-- transitive descendants of a node, including itself that lie in a region
-- indicated by another 'Fold'.
cosmosOnOf
  :: (Is k1 A_Fold, Is k2 A_Fold)
  => Optic' k1 is s a
  -> Optic' k2 js a a
  -> Optic' A_Fold is s a
cosmosOnOf d p = castOptic @A_Fold d % cosmosOf p
{-# INLINE cosmosOnOf #-}

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-- | Transform every element in the tree, in a bottom-up manner.
--
-- For example, replacing negative literals with literals:
--
-- @
-- negLits = 'transform' $ \\x -> case x of
--   Neg (Lit i) -> Lit ('negate' i)
--   _           -> x
-- @
transform :: Plated a => (a -> a) -> a -> a
transform = transformOf plate
{-# INLINE transform #-}

-- | Transform every element in the tree in a bottom-up manner over a region
-- indicated by a 'Setter'.
transformOn :: (Is k A_Setter, Plated a) => Optic k is s t a a -> (a -> a) -> s -> t
transformOn b = over b . transform
{-# INLINE transformOn #-}

-- | Transform every element by recursively applying a given 'Setter' in a
-- bottom-up manner.
transformOf :: Is k A_Setter => Optic k is a b a b -> (b -> b) -> a -> b
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

-- | Transform every element in a region indicated by a 'Setter' by recursively
-- applying another 'Setter' in a bottom-up manner.
transformOnOf
  :: (Is k1 A_Setter, Is k2 A_Setter)
  => Optic k1 is1 s t a b
  -> Optic k2 is2 a b a b
  -> (b -> b) -> s -> t
transformOnOf b l = over b . transformOf l
{-# INLINE transformOnOf #-}

-- | Transform every element in the tree, in a bottom-up manner, monadically.
transformM :: (Plated a, Monad m) => (a -> m a) -> a -> m a
transformM = transformMOf plate
{-# INLINE transformM #-}

-- | Transform every element in the tree in a region indicated by a supplied
-- 'Traversal', in a bottom-up manner, monadically.
transformMOn
  :: (Is k A_Traversal, Plated a, Monad m)
  => Optic k is s t a a
  -> (a -> m a) -> s -> m t
transformMOn b = traverseOf b . transformM
{-# INLINE transformMOn #-}

-- | Transform every element in a tree using a user supplied 'Traversal' in a
-- bottom-up manner with a monadic effect.
transformMOf
  :: (Is k A_Traversal, Monad m)
  => Optic k is a b a b
  -> (b -> m b) -> a -> m b
transformMOf l f = go
  where
    go t = traverseOf l go t >>= f
{-# INLINE transformMOf #-}

-- | Transform every element in a tree that lies in a region indicated by a
-- supplied 'Traversal', walking with a user supplied 'Traversal' in a bottom-up
-- manner with a monadic effect.
transformMOnOf
  :: (Is k1 A_Traversal, Is k2 A_Traversal, Monad m)
  => Optic k1 is1 s t a b
  -> Optic k2 is2 a b a b
  -> (b -> m b) -> s -> m t
transformMOnOf b l = traverseOf b . transformMOf l
{-# INLINE transformMOnOf #-}

-------------------------------------------------------------------------------
-- Paramorphisms
-------------------------------------------------------------------------------

-- | Perform a fold-like computation on each value, technically a paramorphism.
paraOf :: Is k A_Fold => Optic' k is a a -> (a -> [r] -> r) -> a -> r
paraOf l f = go
  where
    go a = f a (go <$> toListOf l a)
{-# INLINE paraOf #-}

-- | Perform a fold-like computation on each value, technically a paramorphism.
--
-- @
-- 'para' ≡ 'paraOf' 'plate'
-- @
para :: Plated a => (a -> [r] -> r) -> a -> r
para = paraOf plate
{-# INLINE para #-}

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

-- | The original @uniplate@ combinator, implemented in terms of 'Plated' as a
-- 'Lens'.
--
-- @
-- 'parts' ≡ 'partsOf' 'plate'
-- @
--
-- The resulting 'Lens' is safer to use as it ignores 'over-application' and
-- deals gracefully with under-application, but it is only a proper 'Lens' if
-- you don't change the list 'length'!
parts :: Plated a => Lens' a [a]
parts = partsOf plate
{-# INLINE parts #-}
