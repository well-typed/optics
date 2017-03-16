{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Generics.SOP.Optics
  ( rep
  , sop
  , z
  , i
  , k
  , record
  , npHead
  , npTail
  , npSingleton
  , _Z
  , _S
  )
  where

import Generics.SOP
import Optics hiding (from, to)

-- | Iso between a generic type and its representation.
rep :: (Generic a, Generic b) => Iso a b (Rep a) (Rep b)
rep = iso from to

-- | Iso induced by the 'SOP' newtype.
sop :: Iso (SOP f xss) (SOP g yss) (NS (NP f) xss) (NS (NP g) yss)
sop = iso unSOP SOP

-- | Iso between a one-element sum and its contents.
z :: Iso (NS f '[ x ]) (NS g '[ y ]) (f x) (g y)
z = iso unZ Z

-- | Iso induced by the 'I' newtype.
i :: Iso (I x) (I y) x y
i = iso unI I

-- | Iso induced by the 'K' newtype.
k :: Iso (K a b) (K c d) a c
k = iso unK K

-- | Iso between a generic record type and its product representation.
record :: (Generic a, Generic b, Code a ~ '[ xs ], Code b ~ '[ ys ]) => Iso a b (NP I xs) (NP I ys)
record = rep % sop % z

-- | Lens accessing the head of an 'NP'.
npHead :: Lens (NP f (x ': xs)) (NP f (y ': xs)) (f x) (f y)
npHead = mkLens (\ f (x :* xs) -> (:* xs) <$> f x)

-- | Lens accessing the tail of an 'NP'.
npTail :: Lens (NP f (x ': xs)) (NP f (x ': ys)) (NP f xs) (NP f ys)
npTail = mkLens (\ f (x :* xs) -> (x :*) <$> f xs)

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
