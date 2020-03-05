{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.Label.Test where

import GHC.Generics (Generic)
import Optics.Core
import Optics.Label.Generic ()

data T = MkT { foo :: Int }
  deriving (Show, Generic)

-- e1 :: Int
e1 = view #foo (MkT 42)

-- e2 :: T
e2 = set #foo 3 (MkT undefined)

-- e3 :: T -> T
e3 x = set #foo 11 x :: T


instance (a ~ Int, b ~ Int) => LabelOptic "bar" A_Lens T T a b where
  labelOptic = lensVL $ \ f s -> (\v -> s { foo = v}) <$> f (foo s)

-- e4 :: Int
e4 = view #bar (MkT 42)


data U = MkU { woo :: Int }

e5 :: LabelOptic' "woo" A_Lens U Int => Int
e5 = view #woo (MkU 42)

e6 :: LabelOptic' "foo" A_Lens s a => s -> a
e6 = view #foo

-- e7 :: Int
e7 = e6 (MkT 3)


{-
*Optics.Label.Test> :set -XOverloadedLabels
*Optics.Label.Test> view #missing (MkT 3)

<interactive>:2:6: error:
    • The type T does not contain a field named 'missing'.
    • In the first argument of ‘view’, namely ‘#missing’
      In the expression: view #missing (MkT 3)
      In an equation for ‘it’: it = view #missing (MkT 3)
*Optics.Label.Test> view #missing (MkU 3)

<interactive>:3:1: error:
    • No instance for LabelOptic "missing" ‘k’ ‘U’ ‘U’ ‘a’ ‘a’
        (maybe you forgot to define it or misspelled a name?)
    • When checking the inferred type
        it :: forall (k :: OpticKind) a.
              ((TypeError ...), Is k A_Getter) =>
              a
*Optics.Label.Test> \ x -> set #foo 11 x

<interactive>:4:1: error:
    • No instance for LabelOptic "foo" ‘k’ ‘s’ ‘t’ ‘a’ ‘b’
        (maybe you forgot to define it or misspelled a name?)
    • When checking the inferred type
        it :: forall (k :: OpticKind) s t a b.
              ((TypeError ...), Is k A_Setter, Num b) =>
              s -> t
-}
