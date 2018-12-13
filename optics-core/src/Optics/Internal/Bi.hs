module Optics.Internal.Bi where

import Optics.Internal.Profunctor

class Bifunctor p where
  bimap  :: (a -> b) -> (c -> d) -> p i a c -> p i b d
  first  :: (a -> b)             -> p i a c -> p i b c
  second ::             (c -> d) -> p i a c -> p i a d

class Bicontravariant p where
  contrabimap  :: (b -> a) -> (d -> c) -> p i a c -> p i b d
  contrafirst  :: (b -> a)             -> p i a c -> p i b c
  contrasecond ::             (c -> b) -> p i a b -> p i a c

instance Bicontravariant (Forget r) where
  contrabimap  f _g (Forget r) = Forget (r . f)
  contrafirst  f    (Forget r) = Forget (r . f)
  contrasecond   _g (Forget r) = Forget r
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Bicontravariant (ForgetM r) where
  contrabimap  f _g (ForgetM r) = ForgetM (r . f)
  contrafirst  f    (ForgetM r) = ForgetM (r . f)
  contrasecond   _g (ForgetM r) = ForgetM r
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}

instance Bicontravariant (IxForget r) where
  contrabimap  f _g (IxForget r) = IxForget (\i -> r i . f)
  contrafirst  f    (IxForget r) = IxForget (\i -> r i . f)
  contrasecond   _g (IxForget r) = IxForget r
  {-# INLINE contrabimap #-}
  {-# INLINE contrafirst #-}
  {-# INLINE contrasecond #-}
