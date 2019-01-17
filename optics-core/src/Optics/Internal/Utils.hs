module Optics.Internal.Utils where

import Data.Coerce

data Context a b t = Context (b -> t) a
  deriving Functor

data IxContext i a b t = IxContext (i -> b -> t) a
  deriving Functor

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

(.#) :: Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
(.#) f _g = coerce f
{-# INLINE (.#) #-}
