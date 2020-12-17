{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module: Optics.Indexed
-- Description: Definitions of indexed optics.
--
-- This module defines general functionality for indexed optics.  See the
-- "Indexed optics" section of the overview documentation in the @Optics@ module
-- of the main @optics@ package for more details.
--
-- Unlike "Optics.Indexed.Core", this includes the definitions from modules for
-- specific indexed optic flavours such as "Optics.IxTraversal", and includes
-- additional instances for 'FunctorWithIndex' and similar classes.
--
module Optics.Indexed
  (
    -- * Class for optic kinds that can be indexed
    IxOptic(..)

  , conjoined

    -- * Composition of indexed optics
  , (<%>)
  , (%>)
  , (<%)
  , reindexed
  , icompose
  , icompose3
  , icompose4
  , icompose5
  , icomposeN

    -- * Indexed optic flavours
  , module Optics.IxAffineFold
  , module Optics.IxAffineTraversal
  , module Optics.IxFold
  , module Optics.IxGetter
  , module Optics.IxLens
  , module Optics.IxSetter
  , module Optics.IxTraversal

  -- * Functors with index
  , FunctorWithIndex (..)
  -- ** Foldable with index
  , FoldableWithIndex (..)
  , itraverse_
  , ifor_
  , itoList
  -- ** Traversable with index
  , TraversableWithIndex (..)
  , ifor
  ) where

import Optics.Indexed.Core
import Optics.IxAffineFold
import Optics.IxAffineTraversal
import Optics.IxFold
import Optics.IxGetter
import Optics.IxLens
import Optics.IxSetter
import Optics.IxTraversal

import Data.Functor.WithIndex.Instances ()
