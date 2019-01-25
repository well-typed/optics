module Optics.Iso
  ( An_Iso
  , Iso
  , Iso'
  , toIso
  , iso
  , withIso
  -- * Isomorphisms
  , mapping
  , coerced
  , curried
  , uncurried
  , flipped
  , Swapped(..)
  -- * Re-exports
  , module Optics.Optic
  )
  where

import Data.Tuple
import Data.Bifunctor
import Data.Coerce (Coercible, coerce)

import Optics.Internal.Concrete
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a type-modifying iso.
type Iso s t a b = Optic An_Iso NoIx s t a b

-- | Type synonym for a type-preserving iso.
type Iso' s a = Optic' An_Iso NoIx s a

-- | Explicitly cast an optic to an iso.
toIso :: Is k An_Iso => Optic k is s t a b -> Optic An_Iso is s t a b
toIso = castOptic
{-# INLINE toIso #-}

-- | Build an iso from a pair of inverse functions.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (dimap f g)
{-# INLINE iso #-}

-- | Extract the two components of an isomorphism.
withIso :: Is k An_Iso => Optic k is s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o k = case getOptic (toIso o) (Exchange id id) of
  Exchange sa bt -> k sa bt
{-# INLINE withIso #-}

----------------------------------------
-- Isomorphisms

-- | This can be used to lift any 'Iso' into an arbitrary 'Functor'.
mapping
  :: (Is k An_Iso, Functor f, Functor g)
  => Optic k is s t a b
  -> Iso (f s) (g t) (f a) (g b)
mapping k = withIso k $ \sa bt -> iso (fmap sa) (fmap bt)
{-# INLINE mapping #-}

-- | Data types that are representationally equal are isomorphic.
--
-- >>> view1 coerced 'x' :: Identity Char
-- Identity 'x'
--
coerced :: (Coercible s a, Coercible t b) => Iso s t a b
coerced = iso coerce coerce

-- | The canonical isomorphism for currying and uncurrying a function.
--
-- @
-- 'curried' = 'iso' 'curry' 'uncurry'
-- @
--
-- >>> view1 curried fst 3 4
-- 3
--
curried :: Iso ((a, b) -> c) ((d, e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry
{-# INLINE curried #-}

-- | The canonical isomorphism for uncurrying and currying a function.
--
-- @
-- 'uncurried' = 'iso' 'uncurry' 'curry'
-- @
--
-- @
-- 'uncurried' = 'from' 'curried'
-- @
--
-- >>> (view1 uncurried (+)) (1,2)
-- 3
--
uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a, b) -> c) ((d, e) -> f)
uncurried = iso uncurry curry
{-# INLINE uncurried #-}

-- | The isomorphism for flipping a function.
--
-- >>> (view1 flipped (,)) 1 2
-- (2,1)
--
flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip
{-# INLINE flipped #-}

-- | This class provides for symmetric bifunctors.
class Bifunctor p => Swapped p where
  -- |
  -- @
  -- 'swapped' '.' 'swapped' ≡ 'id'
  -- 'first' f '.' 'swapped' = 'swapped' '.' 'second' f
  -- 'second' g '.' 'swapped' = 'swapped' '.' 'first' g
  -- 'bimap' f g '.' 'swapped' = 'swapped' '.' 'bimap' g f
  -- @
  --
  -- >>> view1 swapped (1,2)
  -- (2,1)
  --
  swapped :: Iso (p a b) (p c d) (p b a) (p d c)

instance Swapped (,) where
  swapped = iso swap swap
  {-# INLINE swapped #-}

instance Swapped Either where
  swapped = iso (either Right Left) (either Right Left)
  {-# INLINE swapped #-}

-- $setup
-- >>> import Optics.Core
-- >>> import Data.Functor.Identity
