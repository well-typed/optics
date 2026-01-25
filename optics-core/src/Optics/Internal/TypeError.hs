{-# LANGUAGE CPP #-}
#if !MIN_VERSION_base(4,19,0)
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
#endif
-- | Internal shim module providing 'Unsatisfiable' constraint
module Optics.Internal.TypeError (
    Unsatisfiable,
    ErrorMessage (..),
) where

#if MIN_VERSION_base(4,19,0)
import GHC.TypeError (Unsatisfiable, ErrorMessage (..))
#else
import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage (..))

#if __GLASGOW_HASKELL__ >= 810
type Unsatisfiable :: ErrorMessage -> Constraint
#endif

-- | A shim definition for 'Unsatisfiable' on older GHCs.
type family Unsatisfiable (e :: ErrorMessage) :: Constraint where
    Unsatisfiable e = TypeError e
#endif
