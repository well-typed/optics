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
  , rewriteM
  , rewriteMOf
  -- ** Universe
  , universe
  , universeOf
  , cosmos
  , cosmosOf
  -- ** Transformation
  , transform
  , transformOf
  , transformM
  , transformMOf
  ) where

import Data.Tree

import Optics.Fold
import Optics.Generic
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

-- | Transform every element by recursively applying a given 'Setter' in a
-- bottom-up manner.
transformOf :: Is k A_Setter => Optic k is a b a b -> (b -> b) -> a -> b
transformOf l f = go where
  go = f . over l go
{-# INLINE transformOf #-}

-- | Transform every element in the tree, in a bottom-up manner, monadically.
transformM :: (Plated a, Monad m) => (a -> m a) -> a -> m a
transformM = transformMOf plate
{-# INLINE transformM #-}

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
