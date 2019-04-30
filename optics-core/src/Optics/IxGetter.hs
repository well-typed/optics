{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module: Optics.IxGetter
-- Description: An indexed version of an 'Optics.Getter.Getter'.
--
-- An 'IxGetter' is an indexed version of an 'Optics.Getter.Getter'. See the "Indexed
-- optics" section of the overview documentation in the @Optics@ module of the
-- main @optics@ package for more details on indexed optics.
--
module Optics.IxGetter
  (
  -- * Formation
    IxGetter

  -- * Introduction
  , ito

  -- * Elimination
  , iview
  , iviews

  -- * Subtyping
  , A_Getter
  , toIxGetter
  ) where

import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Bi

-- | Type synonym for an indexed getter.
type IxGetter i s a = Optic' A_Getter (WithIx i) s a

-- | Explicitly cast an optic to an indexed getter.
toIxGetter
  :: (Is k A_Getter, is `HasSingleIndex` i)
  => Optic' k is s a
  -> IxGetter i s a
toIxGetter = castOptic
{-# INLINE toIxGetter #-}

-- | Build an indexed getter from a function.
--
-- >>> iview (ito id) ('i', 'x')
-- ('i', 'x')
ito :: (s -> (i, a)) -> IxGetter i s a
ito f = Optic (lmap f . ilinear (\g (i, a) -> g i a) . rphantom)

-- | View the value pointed to by an indexed getter.
iview
  :: (Is k A_Getter, is `HasSingleIndex` i)
  => Optic' k is s a -> s -> (i, a)
iview o = iviews o (,)
{-# INLINE iview #-}

-- | View the function of the value pointed to by an indexed getter.
iviews
  :: (Is k A_Getter,  is `HasSingleIndex` i)
  => Optic' k is s a -> (i -> a -> r) -> s -> r
iviews o = \f -> runIxForget (getOptic (toIxGetter o) (IxForget f)) id
{-# INLINE iviews #-}
