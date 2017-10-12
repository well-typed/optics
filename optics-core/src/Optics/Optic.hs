module Optics.Optic
  ( Optic()
  , Optic'
  , Is()
  , sub
  , Join()
  , CanCompose()
  , (%)
  , (%%)
  )
  where

import Optics.Internal.Optic
import Optics.Internal.Subtyping (CanCompose)
