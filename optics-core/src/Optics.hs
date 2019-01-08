-- |
--
-- Module: Optics
-- Description: The main module, usually you only need to import this one.
--
-- Introduction...
--
-- TODO: motivation behind @optics@
--
module Optics
  (
  -- * Basic usage
  -- $basicusage

  -- TODO: explain indexed optics

  -- * Differences from @lens@
  -- $differences

  -- * Core definitions

  -- | "Optics.Optic" module provides core definitions:
  --
  -- * Opaque 'Optic' type,
  --
  -- * which is parameterised over 'OpticKind';
  --
  -- * 'Is' and 'Join' relations, illustrated in the graph below;
  --
  -- * and optic composition operators '%' and '%%'.
  --
  -- <<optics.png Optics hierarchy>>
  --
  -- Red arrows connect optics which can be converted to each other with 're'.
  -- All other arrows represent 'Is' relation (partial order). The hierachy is a 'Join' semilattice, for example the
  -- 'Join' of a 'Lens' and a 'Prism' is an 'AffineTraversal'.
  --
  -- >>> :kind! Join A_Lens A_Prism
  -- Join A_Lens A_Prism :: OpticKind
  -- = 'An_AffineTraversal
  --
  -- There are also indexed variants of 'Traversal', 'Fold' and 'Setter'.
  -- Indexed optics are explained in more detail in /Differences from lens/ section.
  --
    module Optics.Optic

  -- * Optic variants
  
  ,  module O
  
  -- * Optics for concrete base types
  , module P
  )
  where

-- Core optics functionality

-- for some reason haddock reverses the list...

import Optics.Optic

import Optics.Traversal       as O
import Optics.Setter          as O
import Optics.Review          as O
import Optics.Prism           as O
import Optics.Lens            as O
import Optics.IxTraversal     as O
import Optics.IxSetter        as O
import Optics.IxFold          as O
import Optics.Iso             as O
import Optics.Getter          as O
import Optics.Fold            as O
import Optics.Equality        as O
import Optics.AffineTraversal as O
import Optics.AffineFold      as O

-- Optics for concrete base types

import Data.Tuple.Optics  as P
import Data.Maybe.Optics  as P
import Data.Either.Optics as P

-- $basicusage
--
-- @
-- import "Optics"
-- @
--
-- and then...
--
-- Operators (if you prefer them) are in
--
-- @
-- import "Optics.Operators"
-- @
--


-- $differences
--
-- Differences to be listed
