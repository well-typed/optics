module Optics.IxNeFold 
  (
  -- * Formation
    IxNeFold

  -- * Elimination
  , ifoldMap1Of

  -- * Re-exports
  , Foldable1WithIndex(..)
  ) where

import Data.Profunctor.Indexed

import Optics.Internal.Indexed
import Optics.Internal.Indexed.Classes
import Optics.Internal.Optic

-- | Type synonym for an indexed non-empty fold.
type IxNeFold i s a = Optic' A_NeFold (WithIx i) s a
--
-- | Fold with index via embedding into a semigroup.
ifoldMap1Of
  :: (Is k A_NeFold, Semigroup m, is `HasSingleIndex` i)
  => Optic' k is s a
  -> (i -> a -> m) -> s -> m
ifoldMap1Of o = \f -> runIxForget (getOptic (castOptic @A_NeFold o) (IxForget f)) id
