{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.SOP.ToTuple where

import Generics.SOP hiding (from, to)
import Optics

import Generics.SOP.Optics

type family ToTuple (xs :: [*]) :: * where
  ToTuple '[]                                        = ()
  ToTuple '[x1]                                      = x1
  ToTuple '[x1, x2]                                  = (x1, x2)
  ToTuple '[x1, x2, x3]                              = (x1, x2, x3)
  ToTuple '[x1, x2, x3, x4]                          = (x1, x2, x3, x4)
  ToTuple '[x1, x2, x3, x4, x5]                      = (x1, x2, x3, x4, x5)
  ToTuple '[x1, x2, x3, x4, x5, x6]                  = (x1, x2, x3, x4, x5, x6)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7]              = (x1, x2, x3, x4, x5, x6, x7)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8]          = (x1, x2, x3, x4, x5, x6, x7, x8)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9]      = (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  ToTuple '[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)

class TupleLike xs where
  tuple :: Iso' (NP I xs) (ToTuple xs)
  default tuple :: (Generic a, Code a ~ '[ xs ], ToTuple xs ~ a) => Iso' (NP I xs) (ToTuple xs)
  tuple = from record

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

