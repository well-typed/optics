module Optics.Internal.Bicontravariant where

import Optics.Internal.Profunctor

class Bicontravariant p where
  contrabimap  :: (b -> a) -> (d -> c) -> p a c -> p b d
  contrafirst  :: (b -> a) -> p a c -> p b c
  contrasecond :: (c -> b) -> p a b -> p a c

instance Bicontravariant (Forget r) where
  contrabimap  f _g (Forget r) = Forget (r . f)
  contrafirst  f    (Forget r) = Forget (r . f)
  contrasecond   _g (Forget r) = (Forget r)
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Bicontravariant (ForgetM r) where
  contrabimap  f _g (ForgetM r) = ForgetM (r . f)
  contrafirst  f    (ForgetM r) = ForgetM (r . f)
  contrasecond   _g (ForgetM r) = (ForgetM r)
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}
