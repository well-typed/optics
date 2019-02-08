{-# LANGUAGE DataKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal.Generic.Families
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Optics.Internal.Generic.Families
  ( module Families
  , ShowSymbols
  ) where

import Optics.Internal.Generic.Families.Collect   as Families
import Optics.Internal.Generic.Families.Has       as Families
import Optics.Internal.Generic.Families.Changing  as Families

import GHC.TypeLits (ErrorMessage (..), Symbol)

type family ShowSymbols (ctors :: [Symbol]) :: ErrorMessage where
  ShowSymbols '[]
    = 'Text ""
  ShowSymbols (c ': cs)
    = 'Text "• " ':<>: 'Text c ':$$: ShowSymbols cs
