{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
-- | This module defines optics for working with the types in @generics-sop@.
--
module Generics.SOP.Optics
  ( rep
  , sop
  , nsSingleton
  , _I
  , _K
  , productRep
  , npHead
  , npTail
  , npSingleton
  , _Z
  , _S
  )
  where

import Generics.SOP
import Optics.Core hiding (to)

-- | Iso between a generic type and its representation.
rep :: (Generic a, Generic b) => Iso a b (Rep a) (Rep b)
rep = iso from to

-- | Iso induced by the 'SOP' newtype.
sop :: Iso (SOP f xss) (SOP g yss) (NS (NP f) xss) (NS (NP g) yss)
sop = iso unSOP SOP

-- | Iso between a one-element sum and its contents.
nsSingleton :: Iso (NS f '[ x ]) (NS g '[ y ]) (f x) (g y)
nsSingleton = iso unZ Z

-- | Iso induced by the 'I' newtype.
_I :: Iso (I x) (I y) x y
_I = iso unI I

-- | Iso induced by the 'K' newtype.
_K :: Iso (K a b) (K c d) a c
_K = iso unK K

-- | Iso between a generic product type and its product representation.
productRep :: (IsProductType a xs, IsProductType b ys) => Iso a b (NP I xs) (NP I ys)
productRep = rep % sop % nsSingleton

-- | Lens accessing the head of an 'NP'.
npHead :: Lens (NP f (x ': xs)) (NP f (y ': xs)) (f x) (f y)
npHead = lensVL (\ f (x :* xs) -> (:* xs) <$> f x)

-- | Lens accessing the tail of an 'NP'.
npTail :: Lens (NP f (x ': xs)) (NP f (x ': ys)) (NP f xs) (NP f ys)
npTail = lensVL (\ f (x :* xs) -> (x :*) <$> f xs)

-- | Iso between a single-element 'NP' and its contents.
npSingleton :: Iso (NP f '[ x ]) (NP g '[ y ]) (f x) (g y)
npSingleton = iso hd (:* Nil)

-- | Prism for the first option in an 'NS'.
_Z :: Prism (NS f (x ': xs)) (NS f (y ': xs)) (f x) (f y)
_Z = prism Z $ \ ns -> case ns of
  Z x -> Right x
  S y -> Left (S y)

-- | Prism for the other options in an 'NS'.
_S :: Prism (NS f (x ': xs)) (NS f (x ': ys)) (NS f xs) (NS f ys)
_S = prism S $ \ ns -> case ns of
  Z y -> Left (Z y)
  S x -> Right x
