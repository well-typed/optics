{-# LANGUAGE DataKinds #-}
module Optics.IxSetter
  ( A_Setter
  , IxSetter
  , IxSetter'
  , FunctorWithIndex(..)
  , toIxSetter
  , iover
  , iover'
  , iset
  , iset'
  , isets
  , conjoinedSets
  , imapped
  , module Optics.Optic
  ) where

import Optics.Internal.Indexed
import Optics.Internal.IxSetter
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a type-modifying indexed setter.
type IxSetter i s t a b = Optic A_Setter (WithIx i) s t a b

-- | Type synonym for a type-preserving indexed setter.
type IxSetter' i s a = Optic' A_Setter (WithIx i) s a

-- | Explicitly cast an optic to an indexeed setter.
toIxSetter :: Is k A_Setter => Optic k (WithIx i) s t a b -> IxSetter i s t a b
toIxSetter = castOptic
{-# INLINE toIxSetter #-}

-- | Apply an indexed setter as a modifier.
iover
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover o = \f -> runIxFunArrow (getOptic (toIxSetter o) (IxFunArrow f)) id
{-# INLINE iover #-}

-- | Apply an indexed setter as a modifier, strictly.
iover'
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> a -> b) -> s -> t
iover' o = \f ->
  let star = getOptic (toIxSetter o) $ IxStar (\i -> wrapIdentity' . f i)
  in unwrapIdentity' . runIxStar star id

{-# INLINE iover' #-}

-- | Apply an indexed setter.
iset
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> b) -> s -> t
iset o = \f -> iover o (\i _ -> f i)
{-# INLINE iset #-}

-- | Apply an indexed setter, strictly.
iset'
  :: (Is k A_Setter, is `HasSingleIndex` i)
  => Optic k is s t a b
  -> (i -> b) -> s -> t
iset' o = \f -> iover' o (\i _ -> f i)
{-# INLINE iset' #-}

-- | Build an indexed setter from a function to modify the element(s).
isets
  :: ((i -> a -> b) -> s -> t)
  -> IxSetter i s t a b
isets f = Optic (iroam f)
{-# INLINE isets #-}

-- | Build an indexed setter from function to modify the element(s) in both its
-- unindexed and indexed version.
--
-- Appropriate version of the setter will be automatically picked for maximum
-- efficiency depending on whether it is used as indexed or regular one.
--
-- @
-- 'over'  ('conjoinedSets' f g) ≡ 'over'  ('sets' f)
-- 'iover' ('conjoinedFold' f g) ≡ 'iover' ('isets' g)
-- @
conjoinedSets
  :: ((     a -> b) -> s -> t)
  -> ((i -> a -> b) -> s -> t)
  -> IxSetter i s t a b
conjoinedSets f g = Optic (conjoinedSets__ f g)
{-# INLINE conjoinedSets #-}

-- | Indexed setter via the 'FunctorWithIndex' class.
imapped :: FunctorWithIndex i f => IxSetter i (f a) (f b) a b
imapped = Optic imapped__
{-# INLINE imapped #-}
