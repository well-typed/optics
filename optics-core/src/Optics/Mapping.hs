-- |
-- Module: Optics.Mapping
-- Description: Lifting optics using 'Functor's 'map'.
--
-- This module defines 'mapping', which turns an @'Optic'' k 'NoIx' s a@ into an
-- @'Optic'' ('MappedOptic' k) 'NoIx' (f s) (f a)@, in other words optic operating on values
-- in a 'Functor'.
--
{-# LANGUAGE DataKinds #-}
module Optics.Mapping
  ( MappingOptic (..)
  ) where

import Optics.Getter
import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Iso
import Optics.Review

-- $setup
-- >>> import Optics.Core
-- >>> import Optics.Operators

-- | Class for optics supporting 'mapping' through a 'Functor'.
--
-- @since 0.3
class MappingOptic k f g s t a b where
  -- | Type family that maps an optic to the optic kind produced by
  -- 'mapping' using it.
  type MappedOptic k

  -- | The 'Optics.Mapping.mapping' can be used to lift optic through a 'Functor'.
  --
  -- @
  -- 'mapping' :: 'Iso'    s t a b -> 'Iso'    (f s) (g t) (f a) (g b)
  -- 'mapping' :: 'Optics.Lens.Lens'   s   a   -> 'Getter' (f s)       (f a)
  -- 'mapping' :: 'Getter' s   a   -> 'Getter' (f s)       (f a)
  -- 'mapping' :: 'Optics.Prism.Prism'    t   b -> 'Review'       (g t)       (g b)
  -- 'mapping' :: 'Review'   t   b -> 'Review'       (g t)       (g b)
  -- @
  mapping
    :: "mapping" `AcceptsEmptyIndices` is
    => Optic k               is s t a b
    -> Optic (MappedOptic k) is (f s) (g t) (f a) (g b)

instance (Functor f, Functor g) => MappingOptic An_Iso f g s t a b where
  type MappedOptic An_Iso = An_Iso
  mapping k = withIso k $ \sa bt -> iso (fmap sa) (fmap bt)
  {-# INLINE mapping #-}

-- Getter-y optics

-- | 
-- >>> [('a', True), ('b', False)] ^. _1 %& mapping
-- "ab"
--
-- >>> let v = [[ (('a', True), "foo"), (('b', False), "bar")], [ (('c', True), "xyz") ] ]
-- >>> v ^. _1 % _2 %& mapping %& mapping
-- [[True,False],[True]]
--
instance (Functor f, f ~ g, s ~ t, a ~ b) => MappingOptic A_Getter f g s t a b where
  type MappedOptic A_Getter = A_Getter
  mapping o = to (fmap (view o))
  {-# INLINE mapping #-}

instance (Functor f, f ~ g, s ~ t, a ~ b) => MappingOptic A_ReversedPrism f g s t a b where
  type MappedOptic A_ReversedPrism = A_Getter
  mapping o = to (fmap (view o))
  {-# INLINE mapping #-}

instance (Functor f, f ~ g, s ~ t, a ~ b) => MappingOptic A_Lens f g s t a b where
  type MappedOptic A_Lens = A_Getter
  mapping o = to (fmap (view o))
  {-# INLINE mapping #-}

-- Review-y optics

instance (Functor f, f ~ g, s ~ t, a ~ b) => MappingOptic A_Review f g s t a b where
  type MappedOptic A_Review = A_Review
  mapping o = unto (fmap (review o))
  {-# INLINE mapping #-}

instance (Functor f, f ~ g, s ~ t, a ~ b) => MappingOptic A_Prism f g s t a b where
  type MappedOptic A_Prism = A_Review
  mapping o = unto (fmap (review o))
  {-# INLINE mapping #-}

instance (Functor f, f ~ g, s ~ t, a ~ b) => MappingOptic A_ReversedLens f g s t a b where
  type MappedOptic A_ReversedLens = A_Review
  mapping o = unto (fmap (review o))
  {-# INLINE mapping #-}
