module Optics.TH.Internal.Utils where

import Control.Monad
import Data.Maybe
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr
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
quantifyType :: [TyVarBndrSpec] -> Cxt -> Type -> Type
quantifyType = quantifyType' S.empty

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: S.Set Name -> [TyVarBndrSpec] -> Cxt -> Type -> Type
quantifyType' exclude vars cx t = ForallT vs cx t
  where
    vs = filter (\v -> D.tvName v `S.notMember` exclude)
       . changeTVFlags SpecifiedSpec
       . D.freeVariablesWellScoped
       $ map bndrToType vars ++ S.toList (setOf typeVarsKinded t)

    bndrToType = elimTV VarT (\n k -> SigT (VarT n) k)

-- | Pass in a list of lists of extensions, where any of the given extensions
-- will satisfy it. For example, you might need either GADTs or
-- ExistentialQuantification, so you'd write:
--
-- > requireExtensions [[GADTs, ExistentialQuantification]]
--
-- But if you need TypeFamilies and MultiParamTypeClasses, then you'd write:
--
-- > requireExtensions [[TypeFamilies], [MultiParamTypeClasses]]
--
requireExtensions :: String -> [[Extension]] -> Q ()
requireExtensions what extLists = do
  -- Taken from the persistent library
  required <- filterM (fmap (not . or) . traverse isExtEnabled) extLists
  case mapMaybe listToMaybe required of
    [] -> pure ()
    [extension] -> fail $ mconcat
      [ "Generating " ++ what ++ " requires the "
      , show extension
      , " language extension. Please enable it by copy/pasting this line to the top of your file:\n\n"
      , extensionToPragma extension
      , "\n\nTo enable it in a GHCi session, use the following command:\n\n"
      , ":seti -X" ++ show extension
      ]
    extensions -> fail $ mconcat
      [ "Generating " ++ what ++ " requires the following language extensions:\n\n"
      , intercalate "\n" (map (("- " ++) . show) extensions)
      , "\n\nPlease enable the extensions by copy/pasting these lines into the top of your file:\n\n"
      , intercalate "\n" (map extensionToPragma extensions)
      , "\n\nTo enable them in a GHCi session, use the following command:\n\n"
      , ":seti " ++ unwords (map (("-X" ++) . show) extensions)
      ]
  where
    extensionToPragma ext = "{-# LANGUAGE " ++ show ext ++ " #-}"

requireExtensionsForLabels :: Q ()
requireExtensionsForLabels = requireExtensions "LabelOptic instances"
  [ [DataKinds]
  , [FlexibleInstances]
  , [MultiParamTypeClasses]
  , [TypeFamilies, GADTs]
  , [UndecidableInstances]
  ]

requireExtensionsForFields :: Q ()
requireExtensionsForFields = requireExtensions "field optics"
  [ [FlexibleInstances]
  , [FunctionalDependencies]
  ]

------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]
