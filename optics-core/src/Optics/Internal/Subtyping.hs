{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances to implement the subtyping hierarchy between optics.
--
module Optics.Internal.Subtyping where

import GHC.TypeLits (TypeError, ErrorMessage(..))

import Optics.Internal.Equality
import Optics.Internal.Fold
import Optics.Internal.Getter
import Optics.Internal.Iso
import Optics.Internal.Lens
import Optics.Internal.Optic
import Optics.Internal.Traversal
import Optics.Internal.Prism
import Optics.Internal.Review
import Optics.Internal.Setter

-- Instances for A_Fold
--
-- (none)

-- Instances for A_Setter
--
-- (none)

-- Instances for A_Getter

instance Is A_Getter A_Fold where
  implies _ = id

-- Instances for A_Traversal

instance Is A_Traversal A_Fold where
  implies _ = id

instance Is A_Traversal A_Setter where
  implies _ = id

-- Instances for A_Lens

instance Is A_Lens A_Fold where
  implies _ = id

instance Is A_Lens A_Setter where
  implies _ = id

instance Is A_Lens A_Getter where
  implies _ = id

instance Is A_Lens A_Traversal where
  implies _ = id

-- Instances for A_Review
--
-- (none)

-- Instances for A_Prism

instance Is A_Prism A_Fold where
  implies _ = id

instance Is A_Prism A_Setter where
  implies _ = id

instance Is A_Prism A_Review where
  implies _ = id

instance Is A_Prism A_Traversal where
  implies _ = id

-- Instances for An_Iso

instance Is An_Iso A_Fold where
  implies _ = id

instance Is An_Iso A_Setter where
  implies _ = id

instance Is An_Iso A_Getter where
  implies _ = id

instance Is An_Iso A_Traversal where
  implies _ = id

instance Is An_Iso A_Lens where
  implies _ = id

instance Is An_Iso A_Review where
  implies _ = id

instance Is An_Iso A_Prism where
  implies _ = id

-- Instances for An_Equality

instance Is An_Equality A_Fold where
  implies _ = id

instance Is An_Equality A_Setter where
  implies _ = id

instance Is An_Equality A_Getter where
  implies _ = id

instance Is An_Equality A_Traversal where
  implies _ = id

instance Is An_Equality A_Lens where
  implies _ = id

instance Is An_Equality A_Review where
  implies _ = id

instance Is An_Equality A_Prism where
  implies _ = id

instance Is An_Equality An_Iso where
  implies _ = id

-- | Overlappable instance for a custom type error.
instance {-# OVERLAPPABLE #-} TypeError ('ShowType k
                                         ':<>: 'Text " cannot be used as "
                                         ':<>: 'ShowType l
                                        ) => Is k l where
  implies = error "unreachable"

-- Join instances

type family JoinError k l
type instance JoinError k l = TypeError ('ShowType k
                                         ':<>: 'Text " cannot be composed with "
                                         ':<>: 'ShowType l)

type instance Join A_Fold A_Setter    = JoinError A_Fold A_Setter
type instance Join A_Fold A_Getter    = A_Fold
type instance Join A_Fold A_Traversal = A_Fold
type instance Join A_Fold A_Lens      = A_Fold
type instance Join A_Fold A_Review    = JoinError A_Fold A_Review
type instance Join A_Fold A_Prism     = A_Fold
type instance Join A_Fold An_Iso      = A_Fold
type instance Join A_Fold An_Equality = A_Fold

type instance Join A_Setter A_Fold      = JoinError A_Setter A_Fold
type instance Join A_Setter A_Getter    = JoinError A_Setter A_Getter
type instance Join A_Setter A_Traversal = A_Setter
type instance Join A_Setter A_Lens      = A_Setter
type instance Join A_Setter A_Review    = JoinError A_Setter A_Review
type instance Join A_Setter A_Prism     = A_Setter
type instance Join A_Setter An_Iso      = A_Setter
type instance Join A_Setter An_Equality = A_Setter

type instance Join A_Getter A_Fold      = A_Fold
type instance Join A_Getter A_Setter    = JoinError A_Getter A_Setter
type instance Join A_Getter A_Traversal = A_Fold
type instance Join A_Getter A_Lens      = A_Getter
type instance Join A_Getter A_Review    = JoinError A_Getter A_Review
type instance Join A_Getter A_Prism     = A_Fold
type instance Join A_Getter An_Iso      = A_Getter
type instance Join A_Getter An_Equality = A_Getter

type instance Join A_Traversal A_Fold      = A_Fold
type instance Join A_Traversal A_Setter    = A_Setter
type instance Join A_Traversal A_Getter    = A_Fold
type instance Join A_Traversal A_Lens      = A_Traversal
type instance Join A_Traversal A_Review    = JoinError A_Traversal A_Review
type instance Join A_Traversal A_Prism     = A_Traversal
type instance Join A_Traversal An_Iso      = A_Traversal
type instance Join A_Traversal An_Equality = A_Traversal

type instance Join A_Lens A_Fold      = A_Fold
type instance Join A_Lens A_Setter    = A_Setter
type instance Join A_Lens A_Getter    = A_Getter
type instance Join A_Lens A_Traversal = A_Traversal
type instance Join A_Lens A_Review    = JoinError A_Lens A_Review
type instance Join A_Lens A_Prism     = A_Traversal
type instance Join A_Lens An_Iso      = A_Lens
type instance Join A_Lens An_Equality = A_Lens

type instance Join A_Review A_Fold      = JoinError A_Review A_Fold
type instance Join A_Review A_Setter    = JoinError A_Review A_Setter
type instance Join A_Review A_Getter    = JoinError A_Review A_Getter
type instance Join A_Review A_Traversal = JoinError A_Review A_Traversal
type instance Join A_Review A_Lens      = JoinError A_Review A_Lens
type instance Join A_Review A_Prism     = A_Review
type instance Join A_Review An_Iso      = A_Review
type instance Join A_Review An_Equality = A_Review

type instance Join A_Prism A_Fold       = A_Fold
type instance Join A_Prism A_Setter     = A_Setter
type instance Join A_Prism A_Getter     = A_Fold
type instance Join A_Prism A_Traversal  = A_Traversal
type instance Join A_Prism A_Lens       = A_Traversal
type instance Join A_Prism A_Review     = A_Review
type instance Join A_Prism An_Iso       = A_Prism
type instance Join A_Prism An_Equality  = A_Prism

type instance Join An_Iso A_Fold        = A_Fold
type instance Join An_Iso A_Setter      = A_Setter
type instance Join An_Iso A_Getter      = A_Getter
type instance Join An_Iso A_Traversal   = A_Traversal
type instance Join An_Iso A_Lens        = A_Lens
type instance Join An_Iso A_Review      = A_Review
type instance Join An_Iso A_Prism       = A_Prism
type instance Join An_Iso An_Equality   = An_Iso

type instance Join An_Equality A_Fold      = A_Fold
type instance Join An_Equality A_Setter    = A_Setter
type instance Join An_Equality A_Getter    = A_Getter
type instance Join An_Equality A_Traversal = A_Traversal
type instance Join An_Equality A_Lens      = A_Lens
type instance Join An_Equality A_Review    = A_Review
type instance Join An_Equality A_Prism     = A_Prism
type instance Join An_Equality An_Iso      = An_Iso
