{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances to implement the subtyping hierarchy between optics.
--
module Optics.Internal.Subtyping where

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

-- Join instances

instance Join A_Fold      A_Getter     A_Fold
instance Join A_Fold      A_Traversal  A_Fold
instance Join A_Fold      A_Lens       A_Fold
instance Join A_Fold      A_Prism      A_Fold
instance Join A_Fold      An_Iso       A_Fold
instance Join A_Fold      An_Equality  A_Fold

instance Join A_Setter    A_Traversal  A_Setter
instance Join A_Setter    A_Lens       A_Setter
instance Join A_Setter    A_Prism      A_Setter
instance Join A_Setter    An_Iso       A_Setter
instance Join A_Setter    An_Equality  A_Setter

instance Join A_Getter    A_Fold       A_Fold
instance Join A_Getter    A_Traversal  A_Fold
instance Join A_Getter    A_Lens       A_Getter
instance Join A_Getter    A_Prism      A_Fold
instance Join A_Getter    An_Iso       A_Getter
instance Join A_Getter    An_Equality  A_Getter

instance Join A_Traversal A_Fold       A_Fold
instance Join A_Traversal A_Setter     A_Setter
instance Join A_Traversal A_Getter     A_Fold
instance Join A_Traversal A_Lens       A_Traversal
instance Join A_Traversal A_Prism      A_Traversal
instance Join A_Traversal An_Iso       A_Traversal
instance Join A_Traversal An_Equality  A_Traversal

instance Join A_Lens      A_Fold       A_Fold
instance Join A_Lens      A_Setter     A_Setter
instance Join A_Lens      A_Getter     A_Getter
instance Join A_Lens      A_Traversal  A_Traversal
instance Join A_Lens      A_Prism      A_Traversal
instance Join A_Lens      An_Iso       A_Lens
instance Join A_Lens      An_Equality  A_Lens

instance Join A_Review    A_Prism      A_Review
instance Join A_Review    An_Iso       A_Review
instance Join A_Review    An_Equality  A_Review

instance Join A_Prism     A_Fold       A_Fold
instance Join A_Prism     A_Setter     A_Setter
instance Join A_Prism     A_Getter     A_Fold
instance Join A_Prism     A_Traversal  A_Traversal
instance Join A_Prism     A_Lens       A_Traversal
instance Join A_Prism     A_Review     A_Review
instance Join A_Prism     An_Iso       A_Prism
instance Join A_Prism     An_Equality  A_Prism

instance Join An_Iso      A_Fold       A_Fold
instance Join An_Iso      A_Setter     A_Setter
instance Join An_Iso      A_Getter     A_Getter
instance Join An_Iso      A_Traversal  A_Traversal
instance Join An_Iso      A_Lens       A_Lens
instance Join An_Iso      A_Review     A_Review
instance Join An_Iso      A_Prism      A_Prism
instance Join An_Iso      An_Equality  An_Iso

instance Join An_Equality A_Fold       A_Fold
instance Join An_Equality A_Setter     A_Setter
instance Join An_Equality A_Getter     A_Getter
instance Join An_Equality A_Traversal  A_Traversal
instance Join An_Equality A_Lens       A_Lens
instance Join An_Equality A_Review     A_Review
instance Join An_Equality A_Prism      A_Prism
instance Join An_Equality An_Iso       An_Iso



-- | A constraint that can never be satisfied (accompanied by a
-- helpful witness to anything you like).
class Absurd where
  absurd :: a

-- | In order to get nice error messages, we complete the type lattice
-- with a universal supertype, whose constraints can never be
-- satisfied.
data Bogus
type instance Constraints Bogus p f = Absurd
instance Is k Bogus where
  implies = absurd

-- | This typeclass has no instances, and is used so that we get
-- suitable unsolved constraint errors when attempting to compose
-- flavours that do not have a (non-'Bogus') common supertype.
--
-- For example, if you try to compose a fold with a setter then you
-- will get an error
--
-- > Could not deduce (CanCompose A_Fold A_Setter)
class Absurd => CanCompose k l

instance CanCompose A_Fold   A_Setter => Join A_Fold   A_Setter Bogus
instance CanCompose A_Setter A_Fold   => Join A_Setter A_Fold   Bogus
instance CanCompose A_Setter A_Getter => Join A_Setter A_Getter Bogus
instance CanCompose A_Getter A_Setter => Join A_Getter A_Setter Bogus
