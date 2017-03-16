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
rep :: Generic a => Iso' a (Rep a)
rep = iso from to

-- | Iso induced by the 'SOP' newtype.
sop :: Iso' (SOP f xss) (NS (NP f) xss)
sop = iso unSOP SOP

-- | Iso between a one-element sum and its contents.
z :: Iso' (NS f '[ x ]) (f x)
z = iso unZ Z

-- | Iso induced by the 'I' newtype.
i :: Iso' (I x) x
i = iso unI I

-- | Iso induced by the 'K' newtype.
k :: Iso' (K x y) x
k = iso unK K

-- | Iso between a generic record type and its product representation.
record :: (Generic a, Code a ~ '[ xs ]) => Iso' a (NP I xs)
record = rep % sop % z

-- | Lens accessing the head of an 'NP'.
npHead :: Lens' (NP f (x ': xs)) (f x)
npHead = mkLens (\ f (x :* xs) -> (:* xs) <$> f x)

-- | Lens accessing the tail of an 'NP'.
npTail :: Lens' (NP f (x ': xs)) (NP f xs)
npTail = mkLens (\ f (x :* xs) -> (x :*) <$> f xs)

-- | Iso between a single-element 'NP' and its contents.
npSingleton :: Iso' (NP f '[ x ]) (f x)
npSingleton = iso hd (:* Nil) 

-- | Prism for the first option in an 'NS'.
_Z :: Prism' (NS f (x ': xs)) (f x)
_Z = prism Z $ \ ns -> case ns of
  Z x -> Right x
  S _ -> Left ns

-- | Prism for the other options in an 'NS'.
_S :: Prism' (NS f (x ': xs)) (NS f xs)
_S = prism S $ \ ns -> case ns of
  Z _ -> Left ns
  S x -> Right x
