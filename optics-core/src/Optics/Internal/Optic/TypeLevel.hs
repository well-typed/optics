{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.Internal.Optic.TypeLevel where

import Data.Type.Equality
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

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
  Append '[]       '[] = '[]
  Append '[]       ys  = ys
  Append xs        '[] = xs
  Append (x ': xs) ys  = x ': Append xs ys

-- |
--
-- 'AppendProof' is a very simple class which provides a witness
--
-- @
-- foldr f (foldr f init xs) ys = foldr f init (ys ++ xs)
--    where f = (->)
-- @
--
-- However, 'Append' is unsafe variant of 'AppendProof', i.e. doesn't provide
-- the witness; only calculates and restricts the arguments.
--
class AppendProof (xs :: [Type]) (ys :: [Type]) (zs :: [Type]) |
  xs ys -> zs, zs xs -> ys {- , zs ys -> xs -} where
  appendProof :: Proxy i -> Curry xs (Curry ys i) :~: Curry zs i

instance ys ~ zs => AppendProof '[] ys zs where
  appendProof _ = Refl

instance AppendProof xs ys zs => AppendProof (x ': xs) ys (x ': zs) where
  appendProof
    :: forall i. Proxy i
    -> Curry (x ': xs) (Curry ys i) :~: Curry (x ': zs) i
  appendProof i = case appendProof @xs @ys @zs i of
    Refl -> Refl

class CurryCompose xs where
  composeN :: (i -> j) -> Curry xs i -> Curry xs j
    
instance CurryCompose '[] where
  composeN = id
  {-# INLINE composeN #-}

instance CurryCompose xs => CurryCompose (x ': xs) where
  composeN ij f x = composeN @xs ij (f x)
  {-# INLINE composeN #-}
