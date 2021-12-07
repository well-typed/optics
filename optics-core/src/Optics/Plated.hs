module Optics.Plated
  (
  -- * Uniplate
    Plated(..)
  -- * Uniplate combinators
  -- ** Children
  , children
  -- ** Rewriting
  , rewrite
  , rewriteM
  -- ** Universe
  , universe
  , cosmos
  -- ** Transformation
  , transform
  , transformM
  -- ** Paramorphisms
  , para
  -- * Parts
  , parts
  ) where

import Data.Tree

import Optics.Fold
import Optics.Generic
import Optics.Lens
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

-- | Rewrite by applying a monadic rule everywhere you can. Ensures that the
-- rule cannot be applied anywhere in the result.
rewriteM :: (Plated a, Monad m) => (a -> m (Maybe a)) -> a -> m a
rewriteM = rewriteMOf plate
{-# INLINE rewriteM #-}

-------------------------------------------------------------------------------
-- Universe
-------------------------------------------------------------------------------

-- | Retrieve all of the transitive descendants of a 'Plated' container,
-- including itself.
universe :: Plated a => a -> [a]
universe = universeOf plate
{-# INLINE universe #-}

-- | Fold over all transitive descendants of a 'Plated' container, including
-- itself.
cosmos :: Plated a => Fold a a
cosmos = cosmosOf plate
{-# INLINE cosmos #-}

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

-- | Transform every element in the tree, in a bottom-up manner, monadically.
transformM :: (Plated a, Monad m) => (a -> m a) -> a -> m a
transformM = transformMOf plate
{-# INLINE transformM #-}

-------------------------------------------------------------------------------
-- Paramorphisms
-------------------------------------------------------------------------------

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
