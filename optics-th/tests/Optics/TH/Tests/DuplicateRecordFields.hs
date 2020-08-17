{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Test that 'declareFieldLabels' and 'declareLenses' work in the presence of
-- @DuplicateRecordFields@ (see issue #323), including when the data constructor
-- or field name contain a colon, and with data families (because data families
-- are weird).
module Optics.TH.Tests.DuplicateRecordFields where

import Optics.Core
import Optics.TH

$(declareFieldLabels [d|data T = Z | MkT { foo :: Int, (<:) :: Int -> Int }|])
$(declareLenses [d|data U = MkU { foo :: Int, (<:) :: Int -> Int }|])
$(declareLenses [d|data (:::) = (:::) { (>:) :: Int -> Int }|])

foo' :: T -> [Int]
foo' = toListOf #foo

foo'' :: U -> Int
foo'' = view foo

(<::) :: U -> Int -> Int
(<::) = view (<:)

(>::) :: (:::) -> Int -> Int
(>::) = view (>:)

-- NB we cannot use the field name 'foo' here, because there is already a
-- definition of 'foo' as a lens in scope (#338).
$(declareFieldLabels
  [d|data family F x
     data instance F Int = MkF { woo :: Int }|])
$(declareLenses
  [d|data family G x
     data instance G Int = MkG { bar :: Int }|])

foo''' :: F Int -> Int
foo''' = view #woo

bar' :: G Int -> Int
bar' = view bar
