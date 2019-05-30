{-# OPTIONS_HADDOCK not-home #-}

-- | Internal implementation details of folds.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Fold where

import Data.Functor
import Data.Foldable
import Data.Maybe
import qualified Data.Semigroup as SG

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Internal implementation of 'Optics.Fold.mkFold'.
mkFold__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (a -> f u) -> s -> f v)
  -> Optic__ p i ci i ci s t a b
mkFold__ f = rphantom . wander f . rphantom
{-# INLINE mkFold__ #-}

-- | Internal implementation of 'Optics.Fold.folded'.
folded__
  :: (Bicontravariant p, Traversing p, Foldable f)
  => Optic__ p i ci i ci (f a) (f b) a b
folded__ = mkFold__ traverse_
{-# INLINE folded__ #-}

-- | Internal implementation of 'Optics.Fold.foldring'.
foldring__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (a -> f u -> f u) -> f v -> s -> f w)
  -> Optic__ p i ci i ci s t a b
foldring__ fr = mkFold__ $ \f -> void . fr (\a -> (f a *>)) (pure v)
  where
    v = error "foldring__: value used"
{-# INLINE foldring__ #-}

------------------------------------------------------------------------------
-- Leftmost and Rightmost
------------------------------------------------------------------------------

-- | Used for 'Optics.Fold.headOf' and 'Optics.IxFold.iheadOf'.
data Leftmost a = LPure | LLeaf a | LStep (Leftmost a)

instance SG.Semigroup (Leftmost a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Leftmost a) where
  mempty = LPure
  {-# INLINE mempty #-}
  mappend x y = LStep $ case x of
    LPure    -> y
    LLeaf _  -> x
    LStep x' -> case y of
      -- The last two cases make headOf produce a Just as soon as any element is
      -- encountered, and possibly serve as a micro-optimisation; this behaviour
      -- can be disabled by replacing them with _ -> mappend x y'.  Note that
      -- this means that firstOf (backwards folded) [1..] is Just _|_.
      LPure    -> x'
      LLeaf a  -> LLeaf $ fromMaybe a (getLeftmost x')
      LStep y' -> mappend x' y'

-- | Extract the 'Leftmost' element. This will fairly eagerly determine that it
-- can return 'Just' the moment it sees any element at all.
getLeftmost :: Leftmost a -> Maybe a
getLeftmost LPure     = Nothing
getLeftmost (LLeaf a) = Just a
getLeftmost (LStep x) = go x
  where
    -- Make getLeftmost non-recursive so it might be inlined for LPure/LLeaf.
    go LPure     = Nothing
    go (LLeaf a) = Just a
    go (LStep a) = go a

-- | Used for 'Optics.Fold.lastOf' and 'Optics.IxFold.ilastOf'.
data Rightmost a = RPure | RLeaf a | RStep (Rightmost a)

instance SG.Semigroup (Rightmost a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Rightmost a) where
  mempty = RPure
  {-# INLINE mempty #-}
  mappend x y = RStep $ case y of
    RPure    -> x
    RLeaf _  -> y
    RStep y' -> case x of
      -- The last two cases make lastOf produce a Just as soon as any element is
      -- encountered, and possibly serve as a micro-optimisation; this behaviour
      -- can be disabled by replacing them with _ -> mappend x y'.  Note that
      -- this means that lastOf folded [1..] is Just _|_.
      RPure    -> y'
      RLeaf a  -> RLeaf $ fromMaybe a (getRightmost y')
      RStep x' -> mappend x' y'

-- | Extract the 'Rightmost' element. This will fairly eagerly determine that it
-- can return 'Just' the moment it sees any element at all.
getRightmost :: Rightmost a -> Maybe a
getRightmost RPure     = Nothing
getRightmost (RLeaf a) = Just a
getRightmost (RStep x) = go x
  where
    -- Make getRightmost non-recursive so it might be inlined for RPure/RLeaf.
    go RPure     = Nothing
    go (RLeaf a) = Just a
    go (RStep a) = go a
