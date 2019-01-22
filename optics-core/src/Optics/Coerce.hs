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

-- | 'coerceS'' with type arguments rearranged for TypeApplications.
coerceS
  :: (Coercible s s', CoercibleOptic k)
  => Optic k is s  t a b
  -> Optic k is s' t a b
coerceS = coerceS'
{-# INLINE coerceS #-}

-- | 'coerceT'' with type arguments rearranged for TypeApplications.
coerceT
  :: (Coercible t t', CoercibleOptic k)
  => Optic k is s t  a b
  -> Optic k is s t' a b
coerceT = coerceT'
{-# INLINE coerceT #-}

-- | 'coerceA'' with type arguments rearranged for TypeApplications.
coerceA
  :: (Coercible a a', CoercibleOptic k)
  => Optic k is s t a  b
  -> Optic k is s t a' b
coerceA = coerceA'
{-# INLINE coerceA #-}

-- | 'coerceB'' with type arguments rearranged for TypeApplications.
coerceB
  :: (Coercible b b', CoercibleOptic k)
  => Optic k is s t a b
  -> Optic k is s t a b'
coerceB = coerceB'
{-# INLINE coerceB #-}

class CoercibleOptic k where
  coerceS' :: Coercible s s' => Optic k is s t a b -> Optic k is s' t a b
  coerceT' :: Coercible t t' => Optic k is s t a b -> Optic k is s t' a b
  coerceA' :: Coercible a a' => Optic k is s t a b -> Optic k is s t a' b
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
