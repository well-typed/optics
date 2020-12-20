{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Magic where

-- | How about a magic trick? I'm gonna make the coverage condition disappear.
class Dysfunctional field k s t a b | field s -> k t a b
                                    , field t -> k s a b

-- | Show something useful when type inference goes into a loop and stops with
-- "reduction stack overflow" message (sometimes happens when trying to infer
-- types of local bindings when monomorphism restriction is enabled).
instance
  ( TypeInferenceLoop
    "Type inference for the local binding failed. Write the type"
    "signature yourself or disable monomorphism restriction with"
    "NoMonomorphismRestriction LANGUAGE pragma so GHC infers it."
    field k s t a b
  ) => Dysfunctional field k s t a b

class TypeInferenceLoop msg1 msg2 msg3 field k s t a b | field s -> k t a b
                                                       , field t -> k s a b

-- | Including the instance head in the context lifts the coverage condition for
-- all type variables in the instance. A dirty trick until we have
-- https://github.com/ghc-proposals/ghc-proposals/pull/374 and can do it
-- properly.
instance
  ( TypeInferenceLoop msg1 msg2 msg3 field k s t a b
  ) => TypeInferenceLoop msg1 msg2 msg3 field k s t a b
