-- | This module defines operations to 'coerce' the type parameters of optics to
-- a representationally equal type.  For example, if we have
--
-- > newtype MkInt = MkInt Int
--
-- and
--
-- > l :: Lens' S Int
--
-- then
--
-- > coerceA @Int @MkInt l :: Lens' S MkInt
--
module Optics.Coerce
  ( coerceS
  , coerceT
  , coerceA
  , coerceB
  , CoercibleOptic(..)
  ) where

import Data.Coerce

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Lift 'coerce' to the @s@ parameter of an optic.
--
-- This is 'coerceS'' with type arguments rearranged for TypeApplications.
coerceS
  :: (Coercible s s', CoercibleOptic k)
  => Optic k is s  t a b
  -> Optic k is s' t a b
coerceS = coerceS'
{-# INLINE coerceS #-}

-- | Lift 'coerce' to the @t@ parameter of an optic.
--
-- This is 'coerceT'' with type arguments rearranged for TypeApplications.
coerceT
  :: (Coercible t t', CoercibleOptic k)
  => Optic k is s t  a b
  -> Optic k is s t' a b
coerceT = coerceT'
{-# INLINE coerceT #-}

-- | Lift 'coerce' to the @a@ parameter of an optic.
--
-- This is 'coerceA'' with type arguments rearranged for TypeApplications.
coerceA
  :: (Coercible a a', CoercibleOptic k)
  => Optic k is s t a  b
  -> Optic k is s t a' b
coerceA = coerceA'
{-# INLINE coerceA #-}

-- | Lift 'coerce' to the @b@ parameter of an optic.
--
-- This is 'coerceB'' with type arguments rearranged for TypeApplications.
coerceB
  :: (Coercible b b', CoercibleOptic k)
  => Optic k is s t a b
  -> Optic k is s t a b'
coerceB = coerceB'
{-# INLINE coerceB #-}

-- | Class for optics that makes it possible to lift 'coerce' through the type
-- parameters of the optic.
class CoercibleOptic k where
  -- | Lift 'coerce' to the @s@ parameter of an optic.
  coerceS' :: Coercible s s' => Optic k is s t a b -> Optic k is s' t a b

  -- | Lift 'coerce' to the @t@ parameter of an optic.
  coerceT' :: Coercible t t' => Optic k is s t a b -> Optic k is s t' a b

  -- | Lift 'coerce' to the @a@ parameter of an optic.
  coerceA' :: Coercible a a' => Optic k is s t a b -> Optic k is s t a' b

  -- | Lift 'coerce' to the @b@ parameter of an optic.
  coerceB' :: Coercible b b' => Optic k is s t a b -> Optic k is s t a b'

instance CoercibleOptic An_Iso where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Lens where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Prism where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic An_AffineTraversal where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Traversal where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Setter where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_PrismaticGetter where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Getter where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic An_AffineFold where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Fold where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_LensyReview where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

instance CoercibleOptic A_Review where
  coerceS' (Optic o) = Optic (coerceS__ o)
  coerceT' (Optic o) = Optic (coerceT__ o)
  coerceA' (Optic o) = Optic (coerceA__ o)
  coerceB' (Optic o) = Optic (coerceB__ o)
  {-# INLINE coerceS' #-}
  {-# INLINE coerceT' #-}
  {-# INLINE coerceA' #-}
  {-# INLINE coerceB' #-}

----------------------------------------
-- Internal implementation

coerceS__
  :: (Coercible s s', Profunctor p)
  => Optic__ p i j s  t a b
  -> Optic__ p i j s' t a b
coerceS__ o = lcoerce . o

coerceT__
  :: (Coercible t t', Profunctor p)
  => Optic__ p i j s t  a b
  -> Optic__ p i j s t' a b
coerceT__ o = rcoerce . o

coerceA__
  :: (Coercible a a', Profunctor p)
  => Optic__ p i j s t a  b
  -> Optic__ p i j s t a' b
coerceA__ o = o . lcoerce

coerceB__
  :: (Coercible b b', Profunctor p)
  => Optic__ p i j s t a b
  -> Optic__ p i j s t a b'
coerceB__ o = o . rcoerce
