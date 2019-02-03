module Optics.Arrow
  ( ArrowOptic(..)
  , assignA
  ) where

import Control.Arrow
import Data.Coerce
import qualified Control.Category as C

import Optics.AffineTraversal
import Optics.Prism
import Optics.Setter
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

newtype WrappedArrow p i a b = WrapArrow { unwrapArrow :: p a b }

instance C.Category p => C.Category (WrappedArrow p i) where
  WrapArrow f . WrapArrow g = WrapArrow (f C.. g)
  id                        = WrapArrow C.id
  {-# INLINE (.) #-}
  {-# INLINE id #-}

instance Arrow p => Arrow (WrappedArrow p i) where
  arr                         = WrapArrow #. arr
  first                       = WrapArrow #. first  .# unwrapArrow
  second                      = WrapArrow #. second .# unwrapArrow
  WrapArrow a *** WrapArrow b = WrapArrow (a *** b)
  WrapArrow a &&& WrapArrow b = WrapArrow (a &&& b)
  {-# INLINE arr #-}
  {-# INLINE first #-}
  {-# INLINE second #-}
  {-# INLINE (***) #-}
  {-# INLINE (&&&) #-}

instance Arrow p => Profunctor (WrappedArrow p) where
  dimap f g k = arr f >>> k >>> arr g
  lmap  f   k = arr f >>> k
  rmap    g k =           k >>> arr g
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

  lcoerce' = lmap coerce
  rcoerce' = rmap coerce
  {-# INLINE lcoerce' #-}
  {-# INLINE rcoerce' #-}

instance Arrow p => Strong (WrappedArrow p) where
  first'  (WrapArrow k) = WrapArrow (first k)
  second' (WrapArrow k) = WrapArrow (second k)
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance ArrowChoice p => Choice (WrappedArrow p) where
  left'  (WrapArrow k) = WrapArrow (left k)
  right' (WrapArrow k) = WrapArrow (right k)
  {-# INLINE left' #-}
  {-# INLINE right' #-}

class Arrow arr => ArrowOptic k arr where
  -- | Turn an optic into an arrow transformer.
  overA :: Optic k is s t a b -> arr a b -> arr s t

instance Arrow arr => ArrowOptic An_Equality arr where
  overA = overA__
  {-# INLINE overA #-}

instance Arrow arr => ArrowOptic An_Iso arr where
  overA = overA__
  {-# INLINE overA #-}

instance Arrow arr => ArrowOptic A_Lens arr where
  overA = overA__
  {-# INLINE overA #-}

instance ArrowChoice arr => ArrowOptic A_Prism arr where
  overA = overA__
  {-# INLINE overA #-}

instance ArrowChoice arr => ArrowOptic An_AffineTraversal arr where
  overA = overA__
  {-# INLINE overA #-}

-- | Run an arrow command and use the output to set all the targets of an optic
-- to the result.
--
-- @
-- runKleisli action ((), (), ()) where
--   action =      assignA _1 (Kleisli (const getVal1))
--            \>>> assignA _2 (Kleisli (const getVal2))
--            \>>> assignA _3 (Kleisli (const getVal3))
--   getVal1 :: Either String Int
--   getVal1 = ...
--   getVal2 :: Either String Bool
--   getVal2 = ...
--   getVal3 :: Either String Char
--   getVal3 = ...
-- @
--
-- has the type @'Either' 'String' ('Int', 'Bool', 'Char')@
assignA
  :: (Is k A_Setter, Arrow arr)
  => Optic k is s t a b
  -> arr s b -> arr s t
assignA o p = arr (flip $ set o) &&& p >>> arr (uncurry id)
{-# INLINE assignA #-}

----------------------------------------

-- | Internal implementation of overA.
overA__
  :: Constraints k (WrappedArrow arr)
  => Optic k is s t a b
  -> arr a b -> arr s t
overA__ o = unwrapArrow #. getOptic o .# WrapArrow
{-# INLINE overA__ #-}
