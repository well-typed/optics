{-# OPTIONS_HADDOCK not-home #-}

-- | Classes for co- and contravariant bifunctors.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Bi where

import Data.Void

import Optics.Internal.Profunctor

-- | Class for (covariant) bifunctors.
class Bifunctor p where
  bimap  :: (a -> b) -> (c -> d) -> p i ci a c -> p i ci b d
  first  :: (a -> b)             -> p i ci a c -> p i ci b c
  second ::             (c -> d) -> p i ci a c -> p i ci a d

-- | Class for contravariant bifunctors.
class Bicontravariant p where
  contrabimap  :: (b -> a) -> (d -> c) -> p i ci a c -> p i ci b d
  contrafirst  :: (b -> a)             -> p i ci a c -> p i ci b c
  contrasecond ::             (c -> b) -> p i ci a b -> p i ci a c

instance Bicontravariant (Forget r) where
  contrabimap  f _g (Forget k) = Forget (k . f)
  contrafirst  f    (Forget k) = Forget (k . f)
  contrasecond   _g (Forget k) = Forget k
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Bicontravariant (ForgetM r) where
  contrabimap  f _g (ForgetM k) = ForgetM (k . f)
  contrafirst  f    (ForgetM k) = ForgetM (k . f)
  contrasecond   _g (ForgetM k) = ForgetM k
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Bicontravariant (IxForget r) where
  contrabimap  f _g (IxForget k) = IxForget (\i -> k i . f)
  contrafirst  f    (IxForget k) = IxForget (\i -> k i . f)
  contrasecond   _g (IxForget k) = IxForget k
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Bicontravariant (IxForgetM r) where
  contrabimap  f _g (IxForgetM k) = IxForgetM (\i -> k i . f)
  contrafirst  f    (IxForgetM k) = IxForgetM (\i -> k i . f)
  contrasecond   _g (IxForgetM k) = IxForgetM k
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

----------------------------------------

-- | If @p@ is a 'Profunctor' and a 'Bifunctor' then its left parameter must be
-- phantom.
lphantom :: (Profunctor p, Bifunctor p) => p i ci a c -> p i ci b c
lphantom = first absurd . lmap absurd
{-# INLINE lphantom #-}

-- | If @p@ is a 'Profunctor' and 'Bicontravariant' then its right parameter
-- must be phantom.
rphantom :: (Profunctor p, Bicontravariant p) => p i ci c a -> p i ci c b
rphantom = rmap absurd . contrasecond absurd
{-# INLINE rphantom #-}
