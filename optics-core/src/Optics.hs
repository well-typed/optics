module Optics
  ( module O
  , module P
  )
  where

-- Core optics functionality

import Optics.AffineFold      as O
import Optics.AffineTraversal as O
import Optics.Equality        as O
import Optics.Fold            as O
import Optics.Getter          as O
import Optics.Iso             as O
import Optics.Lens            as O
import Optics.Optic           as O ()
import Optics.Prism           as O
import Optics.Review          as O
import Optics.Setter          as O
import Optics.Traversal       as O

-- Optics for concrete base types

import Data.Either.Optics as P
import Data.Maybe.Optics  as P
import Data.Tuple.Optics  as P
