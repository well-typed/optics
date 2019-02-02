-- |
--
-- Module: Optics.Core
-- Description: The core @optics@ functionality re-exported.
--
-- Introduction...
--
-- TODO: motivation behind @optics@
--
module Optics.Core
  ( module O
  , module D
  )
  where

import Optics.AffineFold      as O
import Optics.AffineTraversal as O
import Optics.Arrow           as O
import Optics.Coerce          as O
import Optics.Equality        as O
import Optics.Fold            as O
import Optics.Getter          as O
import Optics.Indexed.Core    as O
import Optics.Iso             as O
import Optics.IxFold          as O
import Optics.IxSetter        as O
import Optics.IxTraversal     as O
import Optics.Lens            as O
import Optics.LensyReview     as O
import Optics.Passthrough     as O
import Optics.Prism           as O
import Optics.PrismaticGetter as O
import Optics.Re              as O
import Optics.Review          as O
import Optics.Setter          as O
import Optics.Traversal       as O
import Optics.View            as O

import Data.Either.Optics     as D
import Data.Maybe.Optics      as D
import Data.Tuple.Optics      as D
