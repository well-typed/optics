{-# OPTIONS_HADDOCK not-home #-}

-- | Classes for co- and contravariant bifunctors.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Bi where

import Data.Coerce
import Data.Void

import Data.Profunctor.Indexed

-- | Class for (covariant) bifunctors.
class Bifunctor p where
  bimap  :: (a -> b) -> (c -> d) -> p i a c -> p i b d
  first  :: (a -> b)             -> p i a c -> p i b c
  second ::             (c -> d) -> p i a c -> p i a d

instance Bifunctor Tagged where
  bimap  _f g = Tagged #. g .# unTagged
  first  _f   = coerce
  second    g = Tagged #. g .# unTagged

-- | Class for contravariant bifunctors.
class Bicontravariant p where
  contrabimap  :: (b -> a) -> (d -> c) -> p i a c -> p i b d
  contrafirst  :: (b -> a)             -> p i a c -> p i b c
  contrasecond ::             (c -> b) -> p i a b -> p i a c

instance Bicontravariant (Forget r) where
  contrabimap  f _g (Forget k) = Forget (k . f)
  contrafirst  f    (Forget k) = Forget (k . f)
  contrasecond   _g (Forget k) = Forget k

instance Bicontravariant (ForgetM r) where
  contrabimap  f _g (ForgetM k) = ForgetM (k . f)
  contrafirst  f    (ForgetM k) = ForgetM (k . f)
  contrasecond   _g (ForgetM k) = ForgetM k

instance Bicontravariant (IxForget r) where
  contrabimap  f _g (IxForget k) = IxForget (\i -> k i . f)
  contrafirst  f    (IxForget k) = IxForget (\i -> k i . f)
  contrasecond   _g (IxForget k) = IxForget k

instance Bicontravariant (IxForgetM r) where
  contrabimap  f _g (IxForgetM k) = IxForgetM (\i -> k i . f)
  contrafirst  f    (IxForgetM k) = IxForgetM (\i -> k i . f)
  contrasecond   _g (IxForgetM k) = IxForgetM k

----------------------------------------

-- | If @p@ is a 'Profunctor' and a 'Bifunctor' then its left parameter must be
-- phantom.
lphantom :: (Profunctor p, Bifunctor p) => p i a c -> p i b c
lphantom = first absurd . lmap absurd

-- | If @p@ is a 'Profunctor' and 'Bicontravariant' then its right parameter
-- must be phantom.
rphantom :: (Profunctor p, Bicontravariant p) => p i c a -> p i c b
rphantom = rmap absurd . contrasecond absurd
