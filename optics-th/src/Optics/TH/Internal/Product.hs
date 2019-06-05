{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module Optics.TH.Internal.Product
  ( LensRules(..)
  , FieldNamer
  , DefName(..)
  , ClassyNamer
  , makeFieldOptics
  , makeFieldOpticsForDec
  , makeFieldOpticsForDec'
  , makeFieldLabelsWith
  , makeFieldLabelsForDec
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
import Data.Set.Optics
import Language.Haskell.TH.Optics.Internal
import Optics.Core hiding (cons)
import Optics.TH.Internal.Utils

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
       T.sequenceA (M.fromSet (buildScaffold True rules s defCons) allDefs)

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

makeFieldLabelsForDec :: LensRules -> Dec -> DecsQ
makeFieldLabelsForDec rules = makeFieldLabelsForDatatype rules <=< D.normalizeDec

-- | Build field optics as labels with a custom configuration.
makeFieldLabelsWith :: LensRules -> Name -> DecsQ
makeFieldLabelsWith rules = D.reifyDatatype >=> makeFieldLabelsForDatatype rules

-- | Compute the field optics for a deconstructed datatype Dec
-- When possible build an Iso otherwise build one optic per field.
makeFieldLabelsForDatatype :: LensRules -> D.DatatypeInfo -> Q [Dec]
makeFieldLabelsForDatatype rules info =
  do perDef <- do
       fieldCons <- traverse normalizeConstructor cons
       let allFields  = toListOf (folded % _2 % folded % _1 % folded) fieldCons
       let defCons    = over normFieldLabels (expandName allFields) fieldCons
           allDefs    = setOf (normFieldLabels % folded) defCons
       T.sequenceA (M.fromSet (buildScaffold False rules s defCons) allDefs)

     let defs = filter isRank1 $ M.toList perDef
     traverse (makeFieldLabel rules) defs

  where
    -- LabelOptic doesn't support higher rank fields because of functional
    -- dependencies (s -> a, t -> b), so just skip them.
    isRank1 = \case
      (_, (OpticSa rank1 _ _ _ _, _)) -> rank1
      _                               -> True

    tyName = D.datatypeName info
    s      = D.datatypeType info
    cons   = D.datatypeCons info

    -- Traverse the field labels of a normalized constructor
    normFieldLabels :: Traversal [(Name,[(a,Type)])] [(Name,[(b,Type)])] a b
    normFieldLabels = traversed % _2 % traversed % _1

    -- Map a (possibly missing) field's name to zero-to-many optic definitions
    expandName :: [Name] -> Maybe Name -> [DefName]
    expandName allFields = concatMap (_fieldToDef rules tyName allFields) . maybeToList

makeFieldLabel
  :: LensRules
  -> (DefName, (OpticStab, [(Name, Int, [Int])]))
  -> Q Dec
makeFieldLabel rules (defName, (defType, cons)) = do
  (context, instHead) <- case defType of
    OpticSa _ _ otype s a -> do
      (a', cxtA) <- eqSubst a "a"
      (b', cxtB) <- eqSubst a "b"
      pure (pure [cxtA, cxtB], pure $ conAppsT ''LabelOptic
        [LitT (StrTyLit fieldName), ConT $ opticTypeToTag otype, s, s, a', b'])
    OpticStab otype s t a b -> do
      ambiguousTypeFamilies <- containsAmbiguousTypeFamilyApplications s a
      -- If 'a' contains ambiguous type family applications, generate type
      -- preserving version as functional dependencies on LabelOptic demand it.
      let t' = if ambiguousTypeFamilies then s else t
      (a', cxtA) <- eqSubst a "a"
      (b', cxtB) <- if ambiguousTypeFamilies
                    then eqSubst a "b"
                    else eqSubst b "b"
      pure (pure [cxtA, cxtB], pure $ conAppsT ''LabelOptic
        [LitT (StrTyLit fieldName), ConT $ opticTypeToTag otype, s, t', a', b'])
  instanceD context instHead (fun 'labelOptic)
  where
    opticTypeToTag AffineFoldType      = ''An_AffineFold
    opticTypeToTag AffineTraversalType = ''An_AffineTraversal
    opticTypeToTag FoldType            = ''A_Fold
    opticTypeToTag GetterType          = ''A_Getter
    opticTypeToTag IsoType             = ''An_Iso
    opticTypeToTag LensType            = ''A_Lens
    opticTypeToTag TraversalType       = ''A_Traversal

    -- TODO: check for injectivity of encountered type families.
    containsAmbiguousTypeFamilyApplications s a = do
      -- We consider type family application ambiguous only if it's applied to a
      -- type variable not referenced anywhere else.
      (hasTypeFamilies, bareVars) <- (`runStateT` setOf typeVars s) $
        go =<< lift (D.resolveTypeSynonyms a)
      pure $ hasTypeFamilies && not (S.null bareVars)
      where
        go (ConT nm) = has (_FamilyI % _1 % _TypeFamilyD) <$> lift (reify nm)
        go (VarT n)  = modify' (S.delete n) *> pure False
        go ty        = or <$> traverse go (toListOf typeSelf ty)

    fieldName = case defName of
      TopName fname      -> nameBase fname
      MethodName _ fname -> nameBase fname

    fun :: Name -> [DecQ]
    fun n = funD n [funDef] : inlinePragma n

    funDef :: ClauseQ
    funDef = makeFieldClause rules (stabToOpticType defType) cons

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
  Bool                       {- ^ allow change of phantom type parameters -} ->
  LensRules                                                                  ->
  Type                              {- ^ outer type                       -} ->
  [(Name, [([DefName], Type)])]     {- ^ normalized constructors          -} ->
  DefName                           {- ^ target definition                -} ->
  Q (OpticStab, [(Name, Int, [Int])])
              {- ^ optic type, definition type, field count, target fields -}
buildScaffold allowPhantomsChange rules s cons defName =

  do (s',t,a,b) <- buildStab allowPhantomsChange s (concatMap snd consForDef)

     let defType
           | Just (tyvars,cx,a') <- preview _ForallT a =
               let optic | lensCase   = GetterType
                         | affineCase = AffineFoldType
                         | otherwise  = FoldType
               in OpticSa (null tyvars) cx optic s' a'

           -- Getter and Fold are always simple
           | not (_allowUpdates rules) =
               let optic | lensCase   = GetterType
                         | affineCase = AffineFoldType
                         | otherwise  = FoldType
               in OpticSa True [] optic s' a

           -- Generate simple Lens and Traversal where possible
           | _simpleLenses rules || s' == t && a == b =
               let optic | isoCase && _allowIsos rules = IsoType
                         | lensCase                    = LensType
                         | affineCase                  = AffineTraversalType
                         | otherwise                   = TraversalType
               in OpticSa True [] optic s' a

           -- Generate type-changing Lens and Traversal otherwise
           | otherwise =
               let optic | isoCase && _allowIsos rules = IsoType
                         | lensCase                    = LensType
                         | affineCase                  = AffineTraversalType
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

    affineCase :: Bool
    affineCase = all (<= 1) affectedFields

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

data OpticStab = OpticStab        OpticType Type Type Type Type
               | OpticSa Bool Cxt OpticType Type Type

stabToType :: OpticStab -> Type
stabToType (OpticStab  c s t a b) =
  quantifyType [] (opticTypeName True c `conAppsT` [s,t,a,b])
stabToType (OpticSa _ cx c s   a  ) =
  quantifyType cx (opticTypeName False c `conAppsT` [s,a])

stabToContext :: OpticStab -> Cxt
stabToContext OpticStab{}          = []
stabToContext (OpticSa _ cx _ _ _) = cx

stabToOpticType :: OpticStab -> OpticType
stabToOpticType (OpticStab c _ _ _ _) = c
stabToOpticType (OpticSa _ _ c _ _) = c

stabToOptic :: OpticStab -> Name
stabToOptic (OpticStab c _ _ _ _) = opticTypeName True c
stabToOptic (OpticSa _ _ c _ _) = opticTypeName False c

stabToS :: OpticStab -> Type
stabToS (OpticStab _ s _ _ _) = s
stabToS (OpticSa _ _ _ s _) = s

stabToA :: OpticStab -> Type
stabToA (OpticStab _ _ _ a _) = a
stabToA (OpticSa _ _ _ _ a) = a

-- | Compute the s t a b types given the outer type 's' and the
-- categorized field types. Left for fixed and Right for visited.
-- These types are "raw" and will be packaged into an 'OpticStab'
-- shortly after creation.
buildStab :: Bool -> Type -> [Either Type Type] -> Q (Type,Type,Type,Type)
buildStab allowPhantomsChange s categorizedFields = do
  -- compute possible type changes
  sub <- T.sequenceA (M.fromSet (newName . nameBase) unfixedTypeVars)
  let (t, b) = over each (substTypeVars sub) (s, a)
  pure (s, t, a, b)
  where
    -- Just take the type of the first field and let GHC do the unification.
    a = fromMaybe
      (error "buildStab: unexpected empty list of fields")
      (preview _head targetFields)

    phantomTypeVars =
      let allTypeVars = folded % chosen % typeVars
      in setOf typeVars s S.\\ setOf allTypeVars categorizedFields

    (fixedFields, targetFields) = partitionEithers categorizedFields
    unfixedTypeVars =
      let fixedTypeVars = setOf typeVars fixedFields
      in if allowPhantomsChange
         then setOf typeVars s S.\\ fixedTypeVars
         else setOf typeVars s S.\\ fixedTypeVars S.\\ phantomTypeVars

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
  c <- newName "c"
  let vars = toListOf typeVars s
      fd   | null vars = []
           | otherwise = [FunDep [c] vars]


  classD (cxt[]) className (map PlainTV (c:vars)) fd
    $ sigD methodName (return (''Lens' `conAppsT` [VarT c, s]))
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
                       [VarT c, stabToA stab]
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
    go (ConT nm) = has (_FamilyI % _1 % _TypeFamilyD) <$> reify nm
    go ty = or <$> traverse go (toListOf typeSelf ty)

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
    AffineFoldType      -> makeAffineFoldClause cons
    AffineTraversalType -> makeAffineTraversalClause cons irref
    FoldType            -> makeFoldClause cons
    IsoType             -> makeIsoClause cons irref
    GetterType          -> makeGetterClause cons
    LensType            -> makeLensClause cons irref
    TraversalType       -> makeTraversalClause cons irref
  where
    irref = _lazyPatterns rules && length cons == 1

makeAffineFoldClause :: [(Name, Int, [Int])] -> ClauseQ
makeAffineFoldClause cons = do
  s <- newName "s"
  clause
    []
    (normalB $ appsE
      [ varE 'afolding
      , lamE [varP s] $ caseE (varE s)
        [ makeAffineFoldMatch conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []
  where
    makeAffineFoldMatch conName fieldCount fields = do
      xs <- newNames "x" $ length fields

      let args = foldr (\(i, x) -> set (ix i) (varP x))
                       (replicate fieldCount wildP)
                       (zip fields xs)

          body = case xs of
            -- Con _ .. _ -> Nothing
            []  -> conE 'Nothing
            -- Con _ .. x_i .. _ -> Just x_i
            [x] -> conE 'Just `appE` varE x
            _   -> error "AffineFold focuses on at most one field"

      match (conP conName args)
            (normalB body)
            []

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

      let args = foldr (\(i, x) -> set (ix i) (varP x))
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
        match (conP conName . set (ix field) (varP x) $ replicate fieldCount wildP)
              (normalB $ varE x)
              []
      _       -> error "Getter focuses on exactly one field"

-- | Build a clause that constructs an Iso.
makeIsoClause :: [(Name, Int, [Int])] -> Bool -> ClauseQ
makeIsoClause fields irref = case fields of
  [(conName, 1, [0])] -> do
    x <- newName "x"
    clause []
           (normalB $ appsE
             [ varE 'iso
             , lamE [irrefP $ conP conName [varP x]] (varE x)
             , conE conName
             ])
           []
  _ -> error "Iso works only for types with one constructor and one field"
  where
    irrefP = if irref then tildeP else id

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
        [ makeLensMatch irrefP f conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []
  where
    irrefP = if irref then tildeP else id

-- | Make a lens match. Used for both lens and affine traversal generation.
makeLensMatch :: (PatQ -> PatQ) -> Name -> Name -> Int -> [Int] -> Q Match
makeLensMatch irrefP f conName fieldCount = \case
  [field] -> do
    xs <- newNames "x" fieldCount
    y  <- newName "y"

    let body = appsE
          [ varE 'fmap
          , lamE [varP y] . appsE $
            conE conName : map varE (set (ix field) y xs)
          , appE (varE f) . varE $ xs !! field
          ]

    -- Con x_1 .. x_n -> fmap (\y_i -> Con x_1 .. y_i .. x_n) (f x_i)
    match (irrefP . conP conName $ map varP xs)
          (normalB body)
          []
  _       -> error "Lens focuses on exactly one field"

makeAffineTraversalClause :: [(Name, Int, [Int])] -> Bool -> ClauseQ
makeAffineTraversalClause cons irref = do
  point <- newName "point"
  f     <- newName "f"
  s     <- newName "s"
  clause
    []
    (normalB $ appsE
      [ varE 'atraversalVL
      , lamE [varP point, varP f, varP s] $ caseE (varE s)
        [ makeAffineTraversalMatch point f conName fieldCount fields
        | (conName, fieldCount, fields) <- cons
        ]
      ])
    []
  where
    irrefP = if irref then tildeP else id

    makeAffineTraversalMatch point f conName fieldCount = \case
      [] -> do
        xs <- newNames "x" fieldCount
        -- Con x_1 ... x_n -> point (Con x_1 .. x_n)
        match (irrefP . conP conName $ map varP xs)
              (normalB $ varE point `appE` appsE (conE conName : map varE xs))
              []
      [field] -> makeLensMatch irrefP f conName fieldCount [field]
      _ -> error "Affine traversal focuses on at most one field"

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
    irrefP = if irref then tildeP else id

    makeTraversalMatch f conName fieldCount fields = do
      xs <- newNames "x" fieldCount
      case fields of
        [] -> -- Con x_1 .. x_n -> pure (Con x_1 .. x_n)
          match (irrefP . conP conName $ map varP xs)
                (normalB $ varE 'pure `appE` appsE (conE conName : map varE xs))
                []
        _ -> do
          ys <- newNames "y" $ length fields

          let xs' = foldr (\(i, x) -> set (ix i) x) xs (zip fields ys)

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
          match (irrefP . conP conName $ map varP xs)
                (normalB body)
                []

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

 -- We want to catch type families, but not *data* families. See #799.
_TypeFamilyD :: AffineFold Dec ()
_TypeFamilyD = _OpenTypeFamilyD % united `afailing` _ClosedTypeFamilyD % united

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
