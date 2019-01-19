{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Internal.FieldTH
-- Copyright   :  (C) 2014-2016 Edward Kmett, (C) 2014 Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Optics.Internal.FieldTH
  ( LensRules(..)
  , FieldNamer
  , DefName(..)
  , ClassyNamer
  , makeFieldOptics
  , makeFieldOpticsForDec
  , makeFieldOpticsForDec'
  , HasFieldClasses
  ) where

import Control.Monad
import Control.Monad.State
import Data.Either
import Data.List
import Data.Maybe
import Language.Haskell.TH
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Traversable as T
import qualified Language.Haskell.TH.Datatype as D

import Data.Either.Optics
import Data.Tuple.Optics
import Language.Haskell.TH.Optics
import Optics.AffineFold
import Optics.AffineTraversal
import Optics.Fold
import Optics.Getter
import Optics.Iso
import Optics.Lens
import Optics.Internal.TH
import Optics.Setter
import Optics.Traversal

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

typeSelf :: Traversal' Type Type
typeSelf = traversalVL $ \f -> \case
  ForallT tyVarBndrs ctx ty ->
    let go (KindedTV nam kind) = KindedTV <$> pure nam <*> f kind
        go (PlainTV nam)       = pure (PlainTV nam)
    in ForallT <$> traverse go tyVarBndrs <*> traverse f ctx <*> f ty
  AppT ty1 ty2              -> AppT <$> f ty1 <*> f ty2
  SigT ty kind              -> SigT <$> f ty <*> f kind
  InfixT ty1 nam ty2        -> InfixT <$> f ty1 <*> pure nam <*> f ty2
  UInfixT ty1 nam ty2       -> UInfixT <$> f ty1 <*> pure nam <*> f ty2
  ParensT ty                -> ParensT <$> f ty
  ty                        -> pure ty

rewriteOf :: Is k A_Setter => Optic k is a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))

transformOf :: Is k A_Setter => Optic k is a b a b -> (b -> b) -> a -> b
transformOf l f = go where
  go = f . over l go

setIx :: Int -> a -> [a] -> [a]
setIx n x xs = case splitAt n xs of
  (xss, [])       -> xss
  (xss, _ : rest) -> xss ++ (x : rest)

------------------------------------------------------------------------
-- Field generation entry point
------------------------------------------------------------------------


-- | Compute the field optics for the type identified by the given type name.
-- Lenses will be computed when possible, Traversals otherwise.
makeFieldOptics :: LensRules -> Name -> DecsQ
makeFieldOptics rules = (`evalStateT` S.empty) . makeFieldOpticsForDatatype rules <=< D.reifyDatatype

makeFieldOpticsForDec :: LensRules -> Dec -> DecsQ
makeFieldOpticsForDec rules = (`evalStateT` S.empty) . makeFieldOpticsForDec' rules

makeFieldOpticsForDec' :: LensRules -> Dec -> HasFieldClasses [Dec]
makeFieldOpticsForDec' rules = makeFieldOpticsForDatatype rules <=< lift . D.normalizeDec

-- | Compute the field optics for a deconstructed datatype Dec
-- When possible build an Iso otherwise build one optic per field.
makeFieldOpticsForDatatype :: LensRules -> D.DatatypeInfo -> HasFieldClasses [Dec]
makeFieldOpticsForDatatype rules info =
  do perDef <- lift $ do
       fieldCons <- traverse normalizeConstructor cons
       let allFields  = toListOf (folded % _2 % folded % _1 % folded) fieldCons
       let defCons    = over normFieldLabels (expandName allFields) fieldCons
           allDefs    = setOf (normFieldLabels % folded) defCons
       T.sequenceA (M.fromSet (buildScaffold rules s defCons) allDefs)

     let defs = M.toList perDef
     case _classyLenses rules tyName of
       Just (className, methodName) ->
         makeClassyDriver rules className methodName s defs
       Nothing -> do decss <- traverse (makeFieldOptic rules) defs
                     return (concat decss)

  where
  tyName = D.datatypeName info
  s      = D.datatypeType info
  cons   = D.datatypeCons info

  -- Traverse the field labels of a normalized constructor
  normFieldLabels :: Traversal [(Name,[(a,Type)])] [(Name,[(b,Type)])] a b
  normFieldLabels = traversed % _2 % traversed % _1

  -- Map a (possibly missing) field's name to zero-to-many optic definitions
  expandName :: [Name] -> Maybe Name -> [DefName]
  expandName allFields = concatMap (_fieldToDef rules tyName allFields) . maybeToList

-- | Normalized the Con type into a uniform positional representation,
-- eliminating the variance between records, infix constructors, and normal
-- constructors.
normalizeConstructor ::
  D.ConstructorInfo ->
  Q (Name, [(Maybe Name, Type)]) -- ^ constructor name, field name, field type

normalizeConstructor con =
  return (D.constructorName con,
          zipWith checkForExistentials fieldNames (D.constructorFields con))
  where
    fieldNames =
      case D.constructorVariant con of
        D.RecordConstructor xs -> fmap Just xs
        D.NormalConstructor    -> repeat Nothing
        D.InfixConstructor     -> repeat Nothing

    -- Fields mentioning existentially quantified types are not
    -- elligible for TH generated optics.
    checkForExistentials _ fieldtype
      | any (\tv -> D.tvName tv `S.member` used) unallowable
      = (Nothing, fieldtype)
      where
        used        = setOf typeVars fieldtype
        unallowable = D.constructorVars con
    checkForExistentials fieldname fieldtype = (fieldname, fieldtype)

-- | Compute the positional location of the fields involved in
-- each constructor for a given optic definition as well as the
-- type of clauses to generate and the type to annotate the declaration
-- with.
buildScaffold ::
  LensRules                                                                  ->
  Type                              {- ^ outer type                       -} ->
  [(Name, [([DefName], Type)])]     {- ^ normalized constructors          -} ->
  DefName                           {- ^ target definition                -} ->
  Q (OpticStab, [(Name, Int, [Int])])
              {- ^ optic type, definition type, field count, target fields -}
buildScaffold rules s cons defName =

  do (s',t,a,b) <- buildStab s (concatMap snd consForDef)

     let defType
           | Just (_,cx,a') <- preview _ForallT a =
               let optic | lensCase   = GetterType
--                         | affineCase = AffineFoldType
                         | otherwise  = FoldType
               in OpticSa cx optic s' a'

           -- Getter and Fold are always simple
           | not (_allowUpdates rules) =
               let optic | lensCase   = GetterType
--                         | affineCase = AffineFoldType
                         | otherwise  = FoldType
               in OpticSa [] optic s' a

           -- Generate simple Lens and Traversal where possible
           | _simpleLenses rules || s' == t && a == b =
               let optic | isoCase && _allowIsos rules = IsoType
                         | lensCase                    = LensType
--                         | affineCase                  = AffineTraversalType
                         | otherwise                   = TraversalType
               in OpticSa [] optic s' a

           -- Generate type-changing Lens and Traversal otherwise
           | otherwise =
               let optic | isoCase && _allowIsos rules = IsoType
                         | lensCase                    = LensType
--                         | affineCase                  = AffineTraversalType
                         | otherwise                   = TraversalType
               in OpticStab optic s' t a b

     return (defType, scaffolds)
  where
    consForDef :: [(Name, [Either Type Type])]
    consForDef = over (mapped % _2 % mapped) categorize cons

    scaffolds :: [(Name, Int, [Int])]
    scaffolds = [ (n, length ts, rightIndices ts) | (n,ts) <- consForDef ]

    rightIndices :: [Either Type Type] -> [Int]
    rightIndices = findIndices (has _Right)

    -- Right: types for this definition
    -- Left : other types
    categorize :: ([DefName], Type) -> Either Type Type
    categorize (defNames, t)
      | defName `elem` defNames = Right t
      | otherwise               = Left  t

    affectedFields :: [Int]
    affectedFields = toListOf (folded % _3 % to length) scaffolds

    lensCase :: Bool
    lensCase = all (== 1) affectedFields

    _affineCase :: Bool
    _affineCase = all (<= 1) affectedFields

    isoCase :: Bool
    isoCase = case scaffolds of
                [(_,1,[0])] -> True
                _           -> False

data OpticType
  = AffineFoldType
  | AffineTraversalType
  | FoldType
  | GetterType
  | IsoType
  | LensType
  | TraversalType
  deriving Show

opticTypeName :: Bool -> OpticType -> Name
opticTypeName typeChanging  AffineTraversalType = if typeChanging
                                                  then ''AffineTraversal
                                                  else ''AffineTraversal'
opticTypeName _typeChanging AffineFoldType      = ''AffineFold
opticTypeName _typeChanging FoldType            = ''Fold
opticTypeName _typeChanging GetterType          = ''Getter
opticTypeName typeChanging  IsoType             = if typeChanging
                                                  then ''Iso
                                                  else ''Iso'
opticTypeName typeChanging  LensType            = if typeChanging
                                                  then ''Lens
                                                  else ''Lens'
opticTypeName typeChanging  TraversalType       = if typeChanging
                                                  then ''Traversal
                                                  else ''Traversal'

data OpticStab = OpticStab     OpticType Type Type Type Type
               | OpticSa   Cxt OpticType Type Type

stabToType :: OpticStab -> Type
stabToType (OpticStab  c s t a b) =
  quantifyType [] (opticTypeName True c `conAppsT` [s,t,a,b])
stabToType (OpticSa cx c s   a  ) =
  quantifyType cx (opticTypeName False c `conAppsT` [s,a])

stabToContext :: OpticStab -> Cxt
stabToContext OpticStab{}        = []
stabToContext (OpticSa cx _ _ _) = cx

stabToOpticType :: OpticStab -> OpticType
stabToOpticType (OpticStab c _ _ _ _) = c
stabToOpticType (OpticSa _ c _ _) = c

stabToOptic :: OpticStab -> Name
stabToOptic (OpticStab c _ _ _ _) = opticTypeName True c
stabToOptic (OpticSa _ c _ _) = opticTypeName False c

stabToS :: OpticStab -> Type
stabToS (OpticStab _ s _ _ _) = s
stabToS (OpticSa _ _ s _) = s

stabToA :: OpticStab -> Type
stabToA (OpticStab _ _ _ a _) = a
stabToA (OpticSa _ _ _ a) = a

-- | Compute the s t a b types given the outer type 's' and the
-- categorized field types. Left for fixed and Right for visited.
-- These types are "raw" and will be packaged into an 'OpticStab'
-- shortly after creation.
buildStab :: Type -> [Either Type Type] -> Q (Type,Type,Type,Type)
buildStab s categorizedFields =
  do (subA,a) <- unifyTypes targetFields
     let s' = applyTypeSubst subA s

     -- compute possible type changes
     sub <- T.sequenceA (M.fromSet (newName . nameBase) unfixedTypeVars)
     let (t,b) = over both (substTypeVars sub) (s',a)

     return (s',t,a,b)

  where
  (fixedFields, targetFields) = partitionEithers categorizedFields
  fixedTypeVars               = setOf typeVars fixedFields
  unfixedTypeVars             = setOf typeVars s S.\\ fixedTypeVars


-- | Build the signature and definition for a single field optic.
-- In the case of a singleton constructor irrefutable matches are
-- used to enable the resulting lenses to be used on a bottom value.
makeFieldOptic ::
  LensRules ->
  (DefName, (OpticStab, [(Name, Int, [Int])])) ->
  HasFieldClasses [Dec]
makeFieldOptic rules (defName, (defType, cons)) = do
  locals <- get
  addName
  lift $ do cls <- mkCls locals
            T.sequenceA (cls ++ sig ++ def)
  where
  mkCls locals = case defName of
                 MethodName c n | _generateClasses rules ->
                  do classExists <- isJust <$> lookupTypeName (show c)
                     return (if classExists || S.member c locals then [] else [makeFieldClass defType c n])
                 _ -> return []

  addName = case defName of
            MethodName c _ -> addFieldClassName c
            _              -> return ()

  sig = case defName of
          _ | not (_generateSigs rules) -> []
          TopName n -> [sigD n (return (stabToType defType))]
          MethodName{} -> []

  fun n = funD n [funDef] : inlinePragma n

  def = case defName of
          TopName n      -> fun n
          MethodName c n -> [makeFieldInstance defType c (fun n)]

  funDef = makeFieldClause rules (stabToOpticType defType) cons

------------------------------------------------------------------------
-- Classy class generator
------------------------------------------------------------------------


makeClassyDriver ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticStab, [(Name, Int, [Int])]))] ->
  HasFieldClasses [Dec]
makeClassyDriver rules className methodName s defs = T.sequenceA (cls ++ inst)

  where
  cls | _generateClasses rules = [lift $ makeClassyClass className methodName s defs]
      | otherwise = []

  inst = [makeClassyInstance rules className methodName s defs]


makeClassyClass ::
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticStab, [(Name, Int, [Int])]))] ->
  DecQ
makeClassyClass className methodName s defs = do
  let ss   = map (stabToS . view (_2 % _1)) defs
  (sub,s') <- unifyTypes (s : ss)
  c <- newName "c"
  let vars = toListOf typeVars s'
      fd   | null vars = []
           | otherwise = [FunDep [c] vars]


  classD (cxt[]) className (map PlainTV (c:vars)) fd
    $ sigD methodName (return (''Lens' `conAppsT` [VarT c, s']))
    : concat
      [ [sigD defName (return ty)
        ,valD (varP defName) (normalB body) []
        ] ++
        inlinePragma defName
      | (TopName defName, (stab, _)) <- defs
      , let body = infixApp (varE methodName) (varE '(%)) (varE defName)
      , let ty   = quantifyType' (S.fromList (c:vars))
                                 (stabToContext stab)
                 $ stabToOptic stab `conAppsT`
                       [VarT c, applyTypeSubst sub (stabToA stab)]
      ]


makeClassyInstance ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticStab, [(Name, Int, [Int])]))] ->
  HasFieldClasses Dec
makeClassyInstance rules className methodName s defs = do
  methodss <- traverse (makeFieldOptic rules') defs

  lift $ instanceD (cxt[]) (return instanceHead)
           $ valD (varP methodName) (normalB (varE 'lensVL `appE` varE 'id)) []
           : map return (concat methodss)

  where
  instanceHead = className `conAppsT` (s : map VarT vars)
  vars         = toListOf typeVars s
  rules'       = rules { _generateSigs    = False
                       , _generateClasses = False
                       }

------------------------------------------------------------------------
-- Field class generation
------------------------------------------------------------------------

makeFieldClass :: OpticStab -> Name -> Name -> DecQ
makeFieldClass defType className methodName =
  classD (cxt []) className [PlainTV s, PlainTV a] [FunDep [s] [a]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (S.fromList [s,a])
                             (stabToContext defType)
             $ stabToOptic defType `conAppsT` [VarT s,VarT a]
  s = mkName "s"
  a = mkName "a"

-- | Build an instance for a field. If the field’s type contains any type
-- families, will produce an equality constraint to avoid a type family
-- application in the instance head.
makeFieldInstance :: OpticStab -> Name -> [DecQ] -> DecQ
makeFieldInstance defType className decs =
  containsTypeFamilies a >>= pickInstanceDec
  where
  s = stabToS defType
  a = stabToA defType

  containsTypeFamilies = go <=< D.resolveTypeSynonyms
    where
    go (ConT nm) = hasTypeFamilyD <$> reify nm
    go ty = or <$> traverse go (toListOf typeSelf ty)

    -- We want to catch type families, but not *data* families. See #799.
    hasTypeFamilyD ty = has (_FamilyI % _1 % _OpenTypeFamilyD) ty
                     || has (_FamilyI % _1 % _ClosedTypeFamilyD) ty

  pickInstanceDec hasFamilies
    | hasFamilies = do
        placeholder <- VarT <$> newName "a"
        mkInstanceDec
          [return (D.equalPred placeholder a)]
          [s, placeholder]
    | otherwise = mkInstanceDec [] [s, a]

  mkInstanceDec context headTys =
    instanceD (cxt context) (return (className `conAppsT` headTys)) decs

------------------------------------------------------------------------
-- Optic clause generators
------------------------------------------------------------------------

makeFieldClause :: LensRules -> OpticType -> [(Name, Int, [Int])] -> ClauseQ
makeFieldClause rules opticType cons =
  case opticType of
    FoldType      -> makeFoldClause cons
    IsoType       -> makeIsoClause cons
    GetterType    -> makeGetterClause cons
    LensType      -> makeLensClause cons irref
    TraversalType -> makeTraversalClause cons irref
    ty            -> error $ show ty
  where
    irref = _lazyPatterns rules && length cons == 1

makeFoldClause :: [(Name, Int, [Int])] -> ClauseQ
makeFoldClause cons = do
  f <- newName "f"
  s <- newName "s"

  clause
    []
    (normalB $ appsE
      [ varE 'foldVL
      , lamE [varP f, varP s] $ caseE (varE s)
        [ makeFoldMatch f conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []
  where
    makeFoldMatch f conName fieldCount fields = do
      xs <- newNames "x" $ length fields

      let args = foldr (\(i, x) -> setIx i (varP x))
                       (replicate fieldCount wildP)
                       (zip fields xs)

          fxs = case xs of
            [] -> [varE 'pure `appE` conE '()]
            _  -> map (\x -> varE f `appE` varE x) xs

          -- Con _ .. x_1 .. _ .. x_k .. _ -> f x_1 *> .. f x_k
          body = appsE
            [ foldr1 (\fx -> infixApp fx (varE '(*>))) fxs
            ]

      match (conP conName args)
            (normalB body)
            []

-- | Build a clause that constructs an Iso.
makeIsoClause :: [(Name, Int, [Int])] -> ClauseQ
makeIsoClause = \case
  [(conName, 1, [0])] ->
    let construct = conE conName
        destruct = do
          x <- newName "x"
          lam1E (conP conName [varP x]) (varE x)
    in clause
         []
         (normalB (appsE [varE 'iso, destruct, construct]))
         []
  _ -> error "Iso works only for types with one constructor and one field"

-- | Build a getter clause that retrieves the field at the given index.
makeGetterClause :: [(Name, Int, [Int])] -> ClauseQ
makeGetterClause cons = do
  s <- newName "s"
  clause
    []
    (normalB $ appsE
      [ varE 'to
      , lamE [varP s] $ caseE (varE s)
        [ makeGetterMatch conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []

  where
    makeGetterMatch conName fieldCount = \case
      [field] -> do
        x <- newName "x"
        -- Con _ .. x_i .. _ -> x_i
        match (conP conName . setIx field (varP x) $ replicate fieldCount wildP)
              (normalB $ varE x)
              []
      _       -> error "Getter focuses on exactly one field"


-- | Build a lens clause that updates the field at the given index. When irref
-- is 'True' the value with be matched with an irrefutable pattern.
makeLensClause :: [(Name, Int, [Int])] -> Bool -> ClauseQ
makeLensClause cons irref = do
  f <- newName "f"
  s <- newName "s"

  clause
    []
    (normalB $ appsE
      [ varE 'lensVL
      , lamE [varP f, varP s] $ caseE (varE s)
        [ makeLensMatch f conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []

  where
    wrap = if irref then tildeP else id

    makeLensMatch f conName fieldCount = \case
      [field] -> do
        xs <- newNames "x" fieldCount
        y  <- newName "y"

        let body = appsE
              [ varE 'fmap
              , lamE [varP y] . appsE $
                conE conName : map varE (setIx field y xs)
              , appE (varE f) . varE $ xs !! field
              ]

        -- Con x_1 .. x_n -> fmap (\y_i -> Con x_1 .. y_i .. x_n) (f x_i)
        match (wrap . conP conName $ map varP xs)
              (normalB body)
              []
      _       -> error "Lens focuses on exactly one field"

makeTraversalClause :: [(Name, Int, [Int])] -> Bool -> ClauseQ
makeTraversalClause cons irref = do
  f <- newName "f"
  s <- newName "s"

  clause
    []
    (normalB $ appsE
      [ varE 'traversalVL
      , lamE [varP f, varP s] $ caseE (varE s)
        [ makeTraversalMatch f conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []
  where
    wrap = if irref then tildeP else id

    makeTraversalMatch f conName fieldCount fields = do
      xs <- newNames "x" fieldCount
      case fields of
        [] -> -- Con x1 .. xn -> pure (Con x1 .. xn)
          match (conP conName (map varP xs))
                (normalB (appE (varE 'pure) (appsE (conE conName : map varE xs))))
                []
        _ -> do
          ys <- newNames "y" $ length fields

          let xs' = foldr (\(i, x) -> setIx i x) xs (zip fields ys)

              mkFx i = varE f `appE` varE (xs !! i)

              body0 = appsE
                [ varE 'pure
                , lamE (map varP ys) $ appsE $ conE conName : map varE xs'
                ]

              body = foldl (\acc i -> infixApp acc (varE '(<*>)) $ mkFx i)
                           body0
                           fields

          -- Con x_1 .. x_n ->
          --  pure (\y_1 .. y_k -> Con x_1 .. y_1 .. x_l .. y_k .. x_n)
          --    <*> f x_i_y_1 <*> .. <*> f x_i_y_k
          match (wrap . conP conName $ map varP xs)
                (normalB body)
                []

------------------------------------------------------------------------
-- Unification logic
------------------------------------------------------------------------

-- The field-oriented optic generation supports incorporating fields
-- with distinct but unifiable types into a single definition.



-- | Unify the given list of types, if possible, and return the
-- substitution used to unify the types for unifying the outer
-- type when building a definition's type signature.
unifyTypes :: [Type] -> Q (M.Map Name Type, Type)
unifyTypes (x:xs) = foldM (uncurry unify1) (M.empty, x) xs
unifyTypes []     = fail "unifyTypes: Bug: Unexpected empty list"


-- | Attempt to unify two given types using a running substitution
unify1 :: M.Map Name Type -> Type -> Type -> Q (M.Map Name Type, Type)
unify1 sub (VarT x) y
  | Just r <- M.lookup x sub = unify1 sub r y
unify1 sub x (VarT y)
  | Just r <- M.lookup y sub = unify1 sub x r
unify1 sub x y
  | x == y = return (sub, x)
unify1 sub (AppT f1 x1) (AppT f2 x2) =
  do (sub1, f) <- unify1 sub  f1 f2
     (sub2, x) <- unify1 sub1 x1 x2
     return (sub2, AppT (applyTypeSubst sub2 f) x)
unify1 sub x (VarT y)
  | elemOf typeVars y (applyTypeSubst sub x) =
      fail "Failed to unify types: occurs check"
  | otherwise = return (M.insert y x sub, x)
unify1 sub (VarT x) y = unify1 sub y (VarT x)

-- TODO: Unify contexts
unify1 sub (ForallT v1 [] t1) (ForallT v2 [] t2) =
     -- This approach works out because by the time this code runs
     -- all of the type variables have been renamed. No risk of shadowing.
  do (sub1,t) <- unify1 sub t1 t2
     v <- fmap nub (traverse (limitedSubst sub1) (v1++v2))
     return (sub1, ForallT v [] t)

unify1 _ x y = fail ("Failed to unify types: " ++ show (x,y))


-- | Perform a limited substitution on type variables. This is used
-- when unifying rank-2 fields when trying to achieve a Getter or Fold.
limitedSubst :: M.Map Name Type -> TyVarBndr -> Q TyVarBndr
limitedSubst sub (PlainTV n)
  | Just r <- M.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (PlainTV m)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst sub (KindedTV n k)
  | Just r <- M.lookup n sub =
       case r of
         VarT m -> limitedSubst sub (KindedTV m k)
         _ -> fail "Unable to unify exotic higher-rank type"
limitedSubst _ tv = return tv


-- | Apply a substitution to a type. This is used after unifying
-- the types of the fields in unifyTypes.
applyTypeSubst :: M.Map Name Type -> Type -> Type
applyTypeSubst sub = rewriteOf typeSelf aux
  where
  aux (VarT n) = M.lookup n sub
  aux _        = Nothing


------------------------------------------------------------------------
-- Field generation parameters
------------------------------------------------------------------------

-- | Rules to construct lenses for data fields.
data LensRules = LensRules
  { _simpleLenses    :: Bool
  , _generateSigs    :: Bool
  , _generateClasses :: Bool
  , _allowIsos       :: Bool
  , _allowUpdates    :: Bool -- ^ Allow Lens/Traversal (otherwise Getter/Fold)
  , _lazyPatterns    :: Bool
  , _fieldToDef      :: FieldNamer
       -- ^ Type Name -> Field Names -> Target Field Name -> Definition Names
  , _classyLenses    :: ClassyNamer
       -- type name to class name and top method
  }

-- | The rule to create function names of lenses for data fields.
--
-- Although it's sometimes useful, you won't need the first two
-- arguments most of the time.
type FieldNamer = Name -- ^ Name of the data type that lenses are being generated for.
                  -> [Name] -- ^ Names of all fields (including the field being named) in the data type.
                  -> Name -- ^ Name of the field being named.
                  -> [DefName] -- ^ Name(s) of the lens functions. If empty, no lens is created for that field.

-- | Name to give to generated field optics.
data DefName
  = TopName Name -- ^ Simple top-level definiton name
  | MethodName Name Name -- ^ makeFields-style class name and method name
  deriving (Show, Eq, Ord)

-- | The optional rule to create a class and method around a
-- monomorphic data type. If this naming convention is provided, it
-- generates a "classy" lens.
type ClassyNamer = Name -- ^ Name of the data type that lenses are being generated for.
                   -> Maybe (Name, Name) -- ^ Names of the class and the main method it generates, respectively.

-- | Tracks the field class 'Name's that have been created so far. We consult
-- these so that we may avoid creating duplicate classes.

-- See #643 for more information.
type HasFieldClasses = StateT (S.Set Name) Q

addFieldClassName :: Name -> HasFieldClasses ()
addFieldClassName n = modify $ S.insert n

------------------------------------------------------------------------
-- Miscellaneous utility functions
------------------------------------------------------------------------


-- | Template Haskell wants type variables declared in a forall, so
-- we find all free type variables in a given type and declare them.
quantifyType :: Cxt -> Type -> Type
quantifyType = quantifyType' S.empty

-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: S.Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = map PlainTV
     $ filter (`S.notMember` exclude)
     $ nub -- stable order
     $ toListOf typeVars t
