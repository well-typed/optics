{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module Optics.TH.Internal.Utils where

import Language.Haskell.TH

-- | Apply arguments to a type constructor
appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

-- | Apply arguments to a function
appsE1 :: ExpQ -> [ExpQ] -> ExpQ
appsE1 = foldl appE

-- | Construct a tuple type given a list of types.
toTupleT :: [TypeQ] -> TypeQ
toTupleT [x] = x
toTupleT xs = appsT (tupleT (length xs)) xs

-- | Construct a tuple value given a list of expressions.
toTupleE :: [ExpQ] -> ExpQ
toTupleE [x] = x
toTupleE xs = tupE xs

-- | Construct a tuple pattern given a list of patterns.
toTupleP :: [PatQ] -> PatQ
toTupleP [x] = x
toTupleP xs = tupP xs

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)

-- | Return 'Name' contained in a 'TyVarBndr'.
bndrName :: TyVarBndr -> Name
bndrName (PlainTV  n  ) = n
bndrName (KindedTV n _) = n

-- | Generate many new names from a given base name.
newNames :: String {- ^ base name -} -> Int {- ^ count -} -> Q [Name]
newNames base n = sequence [ newName (base++show i) | i <- [1..n] ]

------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
