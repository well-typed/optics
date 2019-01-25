{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Optics.Internal.Optic.TypeLevel where

-- | Curry a type-level list.
--
-- In pseudo (dependent-)Haskell:
--
-- @
-- 'Curry' xs y = 'foldr' (->) y xs
-- @
type family Curry (xs :: [*]) (y :: *) :: * where
  Curry '[]       y = y
  Curry (x ': xs) y = x -> Curry xs y

-- | Append two type-level lists together.
type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[]       ys  = ys -- needed for (<%>) and (%>)
  Append xs        '[] = xs -- needed for (<%)
  Append (x ': xs) ys  = x ': Append xs ys

class CurryCompose xs where
  composeN :: (i -> j) -> Curry xs i -> Curry xs j
    
instance CurryCompose '[] where
  composeN = id
  {-# INLINE composeN #-}

instance CurryCompose xs => CurryCompose (x ': xs) where
  composeN ij f x = composeN @xs ij (f x)
  {-# INLINE composeN #-}
