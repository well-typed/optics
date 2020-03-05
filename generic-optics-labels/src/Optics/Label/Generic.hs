{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Optics.Label.Generic
  () where

import Data.Generics.Product.Fields (HasField(field))

import Optics.Internal.Optic

instance (k ~ A_Lens, HasField name s t a b) => GeneralLabelOptic name k s t a b 'True where
  generalLabelOptic = field @name @s @t @a @b
