module Optics.TH.Internal.Utils where

import Data.Maybe
import Language.Haskell.TH
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Haskell.TH.Datatype as D

import Data.Set.Optics
import Language.Haskell.TH.Optics.Internal
import Optics.Core

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

-- We substitute concrete types with type variables and match them with concrete
-- types in the instance context. This significantly improves type inference as
-- GHC can match the instance more easily, but costs dependence on TypeFamilies
-- and UndecidableInstances.
eqSubst :: Type -> String -> Q (Type, Pred)
eqSubst ty n = do
  placeholder <- VarT <$> newName n
  pure (placeholder, D.equalPred placeholder ty)

-- | Fill in kind variables using info from datatype type parameters.
addKindVars :: D.DatatypeInfo -> Type -> Type
addKindVars = substType . M.fromList . mapMaybe var . D.datatypeInstTypes
  where
    var t@(SigT (VarT n) k)
      | has typeVars k = Just (n, t)
      | otherwise      = Nothing
    var _              = Nothing

-- | Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: [TyVarBndr] -> Cxt -> Type -> Type
quantifyType = quantifyType' S.empty

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: S.Set Name -> [TyVarBndr] -> Cxt -> Type -> Type
quantifyType' exclude vars cx t = ForallT vs cx t
  where
    vs = filter (\v -> bndrName v `S.notMember` exclude)
       . D.freeVariablesWellScoped
       $ map bndrToType vars ++ S.toList (setOf typeVarsKinded t)

    bndrToType (PlainTV n)    = VarT n
    bndrToType (KindedTV n k) = SigT (VarT n) k

------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
