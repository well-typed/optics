-- |
--
-- Module: Optics.Core
-- Description: The core optics functionality re-exported.
--
-- See the @Optics@ module in the main @optics@ package for overview
-- documentation.
--
module Optics.Core
  (
  -- * Basic definitions
    module Optics.Optic

  -- * Kinds of optic
  , module O

  -- * Indexed optics
  , module I

  -- * Overloaded labels
  , module Optics.Label

  -- * Combinators
  , module P

  -- * Optics for basic data types
  , module D
  )
  where

import Optics.AffineFold                       as O
import Optics.AffineTraversal                  as O
import Optics.Fold                             as O
import Optics.Getter                           as O
import Optics.Iso                              as O
import Optics.IxAffineFold                     as O
import Optics.IxAffineTraversal                as O
import Optics.IxFold                           as O
import Optics.IxGetter                         as O
import Optics.IxLens                           as O
import Optics.IxSetter                         as O
import Optics.IxTraversal                      as O
import Optics.Lens                             as O
import Optics.ReversedLens                     as O
import Optics.Prism                            as O
import Optics.ReversedPrism                    as O
import Optics.Review                           as O
import Optics.Setter                           as O
import Optics.Traversal                        as O

import Optics.Indexed.Core                     as I

import Optics.Arrow                            as P
import Optics.At.Core                          as P
import Optics.Coerce                           as P
import Optics.Cons.Core                        as P
import Optics.Each.Core                        as P
import Optics.Empty.Core                       as P
import Optics.Generic                          as P
import Optics.Mapping                          as P
import Optics.Operators                        as P
import Optics.Plated                           as P
import Optics.Re                               as P
import Optics.ReadOnly                         as P

import Optics.Label
import Optics.Optic

import Data.Either.Optics                      as D
import Data.Maybe.Optics                       as D
import Data.Tuple.Optics                       as D
