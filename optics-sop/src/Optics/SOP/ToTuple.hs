{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.SOP.ToTuple where

import Generics.SOP hiding (from, to)
import Optics

import Generics.SOP.Optics

type family ToTuple (xs :: [*]) :: * where
  ToTuple '[]                                                                      = ()
  ToTuple '[x1]                                                                    = x1
  ToTuple '[x1, x2]                                                                = (x1, x2)
  ToTuple '[x1, x2, x3]                                                            = (x1, x2, x3)
  ToTuple '[x1, x2, x3, x4]                                                        = (x1, x2, x3, x4)
  ToTuple '[x1, x2, x3, x4, x5]                                                    = (x1, x2, x3, x4, x5)
  ToTuple '[x1, x2, x3, x4, x5, x6]                                                = (x1, x2, x3, x4, x5, x6)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7]                                            = (x1, x2, x3, x4, x5, x6, x7)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8]                                        = (x1, x2, x3, x4, x5, x6, x7, x8)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9]                                    = (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]                               = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11]                          = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12]                     = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]                = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14]           = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15]      = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)

class TupleLike xs where
  tuple :: Iso' i (NP I xs) (ToTuple xs)
  default tuple :: (IsProductType a xs, ToTuple xs ~ a) => Iso' i (NP I xs) (ToTuple xs)
  tuple = re productRep

instance TupleLike '[]
instance TupleLike '[x1] where
  tuple = npSingleton % i
instance TupleLike '[x1, x2]
instance TupleLike '[x1, x2, x3]
instance TupleLike '[x1, x2, x3, x4]
instance TupleLike '[x1, x2, x3, x4, x5]
instance TupleLike '[x1, x2, x3, x4, x5, x6]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15]
instance TupleLike '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16]

