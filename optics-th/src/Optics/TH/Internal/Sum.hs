{-# LANGUAGE TemplateHaskellQuotes #-}
module Optics.TH.Internal.Sum
  ( makePrisms
  , makePrismLabels
  , makeClassyPrisms
  , makeDecPrisms
  ) where

import Data.Char
import Data.Maybe
import Data.Traversable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.Haskell.TH.Datatype as D

import Data.Set.Optics
import Language.Haskell.TH.Optics.Internal
import Optics.Core hiding (cons)
import Optics.Internal.Magic
import Optics.TH.Internal.Utils

-- | Generate a 'Prism' for each constructor of a data type. Isos generated when
-- possible. Reviews are created for constructors with existentially quantified
-- constructors and GADTs.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makePrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- _Foo :: Prism' (FooBarBaz a) Int
-- _Bar :: Prism (FooBarBaz a) (FooBarBaz b) a b
-- _Baz :: Prism' (FooBarBaz a) (Int, Char)
-- @
makePrisms :: Name {- ^ Type constructor name -} -> DecsQ
makePrisms = makePrisms' True

-- | Generate a 'Prism' for each constructor of a data type and combine them
-- into a single class. No Isos are created. Reviews are created for
-- constructors with existentially quantified constructors and GADTs.
--
-- /e.g./
--
-- @
-- data FooBarBaz a
--   = Foo Int
--   | Bar a
--   | Baz Int Char
-- makeClassyPrisms ''FooBarBaz
-- @
--
-- will create
--
-- @
-- class AsFooBarBaz s a | s -> a where
--   _FooBarBaz :: Prism' s (FooBarBaz a)
--   _Foo :: Prism' s Int
--   _Bar :: Prism' s a
--   _Baz :: Prism' s (Int,Char)
--
--   _Foo = _FooBarBaz % _Foo
--   _Bar = _FooBarBaz % _Bar
--   _Baz = _FooBarBaz % _Baz
--
-- instance AsFooBarBaz (FooBarBaz a) a
-- @
--
-- Generate an "As" class of prisms. Names are selected by prefixing the
-- constructor name with an underscore. Constructors with multiple fields will
-- construct Prisms to tuples of those fields.
makeClassyPrisms :: Name {- ^ Type constructor name -} -> DecsQ
makeClassyPrisms = makePrisms' False

makePrismLabels :: Name -> DecsQ
makePrismLabels typeName = do
  requireExtensionsForLabels
  info <- D.reifyDatatype typeName
  let cons = map (normalizeCon info) $ D.datatypeCons info
  catMaybes <$> traverse (makeLabel info cons) cons
  where
    makeLabel :: D.DatatypeInfo -> [NCon] -> NCon -> Q (Maybe Dec)
    makeLabel info cons con = do
      stab@(Stab tvsCovered cx otype s t a b) <- computeOpticType labelConfig ty cons con
      (k,  cxtK) <- eqSubst (ConT $ opticTypeToTag otype) "k"
      (a', cxtA) <- eqSubst a "a"
      (b', cxtB) <- eqSubst b "b"
      let label = nameBase . prismName $ view nconName con
          tyArgs = [LitT (StrTyLit label), k, s, t, a', b']
          context = concat
            [ -- If some of the type variables are not covered, instance is
              -- dysfunctional.
              if tvsCovered then [] else [conAppsT ''Dysfunctional tyArgs]
            , [cxtK, cxtA, cxtB]
            , cx
            ]
      Just <$> instanceD (pure context)
                         (pure $ conAppsT ''LabelOptic tyArgs)
                         (fun stab 'labelOptic)
      where
        ty        = addKindInfo info $ D.datatypeType info
        isNewtype = D.datatypeVariant info == D.Newtype

        opticTypeToTag IsoType    = ''An_Iso
        opticTypeToTag PrismType  = ''A_Prism
        opticTypeToTag ReviewType = ''A_Review -- for complete match

        fun :: Stab -> Name -> [DecQ]
        fun stab n = valD (varP n) (normalB $ funDef stab) [] : inlinePragma n

        funDef :: Stab -> ExpQ
        funDef stab
          | isNewtype = varE 'coerced
          | otherwise = makeConOpticExp stab cons con

-- | Main entry point into Prism generation for a given type constructor name.
makePrisms' :: Bool -> Name -> DecsQ
makePrisms' normal typeName =
  do info <- D.reifyDatatype typeName
     let cls | normal    = Nothing
             | otherwise = Just (D.datatypeName info)
         cons = D.datatypeCons info
     makeConsPrisms info (map (normalizeCon info) cons) cls


-- | Generate prisms for the given 'Dec'
makeDecPrisms :: Bool {- ^ generate top-level definitions -} -> Dec -> DecsQ
makeDecPrisms normal dec =
  do info <- D.normalizeDec dec
     let cls | normal    = Nothing
             | otherwise = Just (D.datatypeName info)
         cons = D.datatypeCons info
     makeConsPrisms info (map (normalizeCon info) cons) cls

-- | Generate prisms for the given type, normalized constructors, and an
-- optional name to be used for generating a prism class. This function
-- dispatches between Iso generation, normal top-level prisms, and classy
-- prisms.
makeConsPrisms :: D.DatatypeInfo -> [NCon] -> Maybe Name -> DecsQ

-- top-level definitions
makeConsPrisms info cons Nothing = fmap concat . for cons $ \con -> do
  stab <- computeOpticType defaultConfig ty cons con
  let n = prismName $ view nconName con
      body = if isNewtype
             then varE 'coerced
             else makeConOpticExp stab cons con
  sequenceA $
    [ sigD n . pure . close $ stabToType stab
    , valD (varP n) (normalB body) []
    ] ++ inlinePragma n
  where
    ty        = addKindInfo info $ D.datatypeType info
    isNewtype = D.datatypeVariant info == D.Newtype

-- classy prism class and instance
makeConsPrisms info cons (Just typeName) =
  sequenceA
    [ makeClassyPrismClass ty className methodName cons
    , makeClassyPrismInstance ty className methodName cons
    ]
  where
    ty = D.datatypeType info
    className = mkName ("As" ++ nameBase typeName)
    methodName = prismName typeName

----------------------------------------

data StabConfig = StabConfig
  { scForLabelInstance :: Bool
  , scAllowIsos        :: Bool
  }

defaultConfig :: StabConfig
defaultConfig = StabConfig
  { scForLabelInstance = False
  , scAllowIsos        = True
  }

classyConfig :: StabConfig
classyConfig = StabConfig
  { scForLabelInstance = False
  , scAllowIsos        = False
  }

labelConfig :: StabConfig
labelConfig = StabConfig
  { scForLabelInstance = True
  , scAllowIsos        = True
  }

data OpticType = IsoType | PrismType | ReviewType
  deriving Eq
data Stab  = Stab Bool Cxt OpticType Type Type Type Type

simplifyStab :: Stab -> Stab
simplifyStab (Stab tvsCovered cx ty _ t _ b) = Stab tvsCovered cx ty t t b b
  -- simplification uses t and b because those types
  -- are interesting in the Review case

stabSimple :: Stab -> Bool
stabSimple (Stab _ _ _ s t a b) = s == t && a == b

stabToType :: Stab -> Type
stabToType stab@(Stab _ cx ty s t a b) = ForallT vs cx $
  case ty of
    IsoType    | stabSimple stab -> ''Iso'    `conAppsT` [s,a]
               | otherwise       -> ''Iso     `conAppsT` [s,t,a,b]
    PrismType  | stabSimple stab -> ''Prism'  `conAppsT` [s,a]
               | otherwise       -> ''Prism   `conAppsT` [s,t,a,b]
    ReviewType                   -> ''Review  `conAppsT` [t,b]

  where
    vs = changeTVFlags SpecifiedSpec
       . D.freeVariablesWellScoped
       . S.toList
       $ setOf (folded % typeVarsKinded) cx

stabType :: Stab -> OpticType
stabType (Stab _ _ o _ _ _ _) = o

computeOpticType :: StabConfig -> Type -> [NCon] -> NCon -> Q Stab
computeOpticType conf t cons con =
  do let cons' = L.delete con cons
     if null (_nconVars con)
         then computePrismType conf t (view nconCxt con) cons' con
         else computeReviewType t (view nconCxt con) (view nconTypes con)

computeReviewType :: Type -> Cxt -> [Type] -> Q Stab
computeReviewType t cx tys = do
  b <- toTupleT (map return tys)
  return (Stab False cx ReviewType t t b b)

-- | Compute the full type-changing Prism type given an outer type, list of
-- constructors, and target constructor name.
computePrismType :: StabConfig -> Type -> Cxt -> [NCon] -> NCon -> Q Stab
computePrismType conf s cx cons con = do
  let ts       = view nconTypes con
      free     = setOf typeVars s
      fixed    = setOf typeVars cons
      phantoms = free S.\\ setOf (folded % nconTypes % typeVars) (con : cons)

      unbound    = free S.\\ fixed
      tvsCovered = if scForLabelInstance conf
                   then S.null phantoms
                   else True
  sub <- sequenceA (M.fromSet (newName . nameBase) unbound)
  a   <- toTupleT (map return ts)
  b   <- toTupleT (map return (substTypeVars sub ts))
  --runIO $ do
  --  putStrLn $ "S:        " ++ show s
  --  putStrLn $ "A:        " ++ show a
  --  putStrLn $ "FREE:     " ++ show free
  --  putStrLn $ "FIXED:    " ++ show fixed
  --  putStrLn $ "PHANTOMS: " ++ show phantoms
  --  putStrLn $ "UNBOUND:  " ++ show unbound
  let t = substTypeVars sub s
      cx' = substTypeVars sub cx
      otype = if null cons && scAllowIsos conf
              then IsoType
              else PrismType
  return (Stab tvsCovered cx' otype s t a b)

-- | Construct either a Review or Prism as appropriate
makeConOpticExp :: Stab -> [NCon] -> NCon -> ExpQ
makeConOpticExp stab cons con =
  case stabType stab of
    IsoType    -> makeConIsoExp con
    PrismType  -> makeConPrismExp stab cons con
    ReviewType -> makeConReviewExp con

-- | Construct prism expression
--
-- prism <<reviewer>> <<remitter>>
makeConPrismExp ::
  Stab ->
  [NCon] {- ^ constructors       -} ->
  NCon   {- ^ target constructor -} ->
  ExpQ
makeConPrismExp stab cons con = appsE [varE 'prism, reviewer, remitter]
  where
  ts = view nconTypes con
  fields  = length ts
  conName = view nconName con

  reviewer                   = makeReviewer       conName fields
  remitter | stabSimple stab = makeSimpleRemitter conName fields
           | otherwise       = makeFullRemitter cons conName


-- | Construct an Iso expression
--
-- iso <<reviewer>> <<remitter>>
makeConIsoExp :: NCon -> ExpQ
makeConIsoExp con = appsE [varE 'iso, remitter, reviewer]
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer    conName fields
  remitter = makeIsoRemitter conName fields


-- | Construct a Review expression
--
-- unto (\(x,y,z) -> Con x y z)
makeConReviewExp :: NCon -> ExpQ
makeConReviewExp con = appE (varE 'unto) reviewer
  where
  conName = view nconName con
  fields  = length (view nconTypes con)

  reviewer = makeReviewer conName fields


------------------------------------------------------------------------
-- Prism and Iso component builders
------------------------------------------------------------------------


-- | Construct the review portion of a prism.
--
-- (\(x,y,z) -> Con x y z) :: b -> t
makeReviewer :: Name -> Int -> ExpQ
makeReviewer conName fields =
  do xs <- newNames "x" fields
     lam1E (toTupleP (map varP xs))
           (conE conName `appsE1` map varE xs)


-- | Construct the remit portion of a prism.
-- Pattern match only target constructor, no type changing
--
-- (\s -> case s of
--          Con x y z -> Right (x,y,z)
--          _         -> Left s
-- ) :: s -> Either s a
makeSimpleRemitter :: Name -> Int -> ExpQ
makeSimpleRemitter conName fields =
  do x  <- newName "x"
     xs <- newNames "y" fields
     let matches =
           [ match (conP conName (map varP xs))
                   (normalB (appE (conE 'Right) (toTupleE (map varE xs))))
                   []
           , match wildP (normalB (appE (conE 'Left) (varE x))) []
           ]
     lam1E (varP x) (caseE (varE x) matches)


-- | Pattern match all constructors to enable type-changing
--
-- (\s -> case s of
--          Con x y z -> Right (x,y,z)
--          Other_n w   -> Left (Other_n w)
-- ) :: s -> Either t a
makeFullRemitter :: [NCon] -> Name -> ExpQ
makeFullRemitter cons target =
  do x <- newName "x"
     lam1E (varP x) (caseE (varE x) (map mkMatch cons))
  where
  mkMatch (NCon conName _ _ n) =
    do xs <- newNames "y" (length n)
       match (conP conName (map varP xs))
             (normalB
               (if conName == target
                  then appE (conE 'Right) (toTupleE (map varE xs))
                  else appE (conE 'Left) (conE conName `appsE1` map varE xs)))
             []


-- | Construct the remitter suitable for use in an 'Iso'
--
-- (\(Con x y z) -> (x,y,z)) :: s -> a
makeIsoRemitter :: Name -> Int -> ExpQ
makeIsoRemitter conName fields =
  do xs <- newNames "x" fields
     lam1E (conP conName (map varP xs))
           (toTupleE (map varE xs))


------------------------------------------------------------------------
-- Classy prisms
------------------------------------------------------------------------


-- | Construct the classy prisms class for a given type and constructors.
--
-- class ClassName r <<vars in type>> | r -> <<vars in Type>> where
--   topMethodName   :: Prism' r Type
--   conMethodName_n :: Prism' r conTypes_n
--   conMethodName_n = topMethodName . conMethodName_n
makeClassyPrismClass ::
  Type   {- Outer type      -} ->
  Name   {- Class name      -} ->
  Name   {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismClass t className methodName cons =
  do r <- newName "r"
     let methodType = appsT (conT ''Prism') [varT r,return t]
     methodss <- traverse (mkMethod (VarT r)) cons'
     classD (cxt[]) className (map plainTV (r : vs)) (fds r)
       ( sigD methodName methodType
       : map return (concat methodss)
       )

  where
  mkMethod r con =
    do Stab tvsCovered cx o _ _ _ b <- computeOpticType classyConfig t cons con
       let stab' = Stab tvsCovered cx o r r b b
           defName = view nconName con
           body    = appsE [varE '(%), varE methodName, varE defName]
       sequenceA
         [ sigD defName        (return (stabToType stab'))
         , valD (varP defName) (normalB body) []
         ]

  cons'         = map (over nconName prismName) cons
  vs            = S.toList (setOf typeVars t)
  fds r
    | null vs   = []
    | otherwise = [FunDep [r] vs]



-- | Construct the classy prisms instance for a given type and constructors.
--
-- instance Classname OuterType where
--   topMethodName = id
--   conMethodName_n = <<prism>>
makeClassyPrismInstance ::
  Type ->
  Name     {- Class name      -} ->
  Name     {- Top method name -} ->
  [NCon] {- Constructors    -} ->
  DecQ
makeClassyPrismInstance s className methodName cons =
  do let vs = S.toList (setOf typeVars s)
         cls = className `conAppsT` (s : map VarT vs)

     instanceD (cxt[]) (return cls)
       (   valD (varP methodName)
                (normalB (varE 'castOptic `appE` varE 'equality)) []
       : [ do stab <- computeOpticType classyConfig s cons con
              let stab' = simplifyStab stab
              valD (varP (prismName conName))
                (normalB (makeConOpticExp stab' cons con)) []
           | con <- cons
           , let conName = view nconName con
           ]
       )


------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------


-- | Normalized constructor
data NCon = NCon
  { _nconName :: Name
  , _nconVars :: [Name]
  , _nconCxt  :: Cxt
  , _nconTypes :: [Type]
  }
  deriving (Eq, Show)

instance HasTypeVars NCon where
  typeVarsEx s = traversalVL $ \f (NCon x vars y z) ->
    let s' = L.foldl' (flip S.insert) s vars
    in NCon x vars <$> traverseOf (typeVarsEx s') f y
                   <*> traverseOf (typeVarsEx s') f z

nconName :: Lens' NCon Name
nconName = lensVL $ \f x -> fmap (\y -> x {_nconName = y}) (f (_nconName x))

nconCxt :: Lens' NCon Cxt
nconCxt = lensVL $ \f x -> fmap (\y -> x {_nconCxt = y}) (f (_nconCxt x))

nconTypes :: Lens' NCon [Type]
nconTypes = lensVL $ \f x -> fmap (\y -> x {_nconTypes = y}) (f (_nconTypes x))


-- | Normalize a single 'Con' to its constructor name and field types.
normalizeCon :: D.DatatypeInfo -> D.ConstructorInfo -> NCon
normalizeCon di info = NCon
  { _nconName  = D.constructorName info
  , _nconVars  = D.tvName <$> D.constructorVars info
  , _nconCxt   = D.constructorContext info
  , _nconTypes = let tyVars = map tyVarBndrToType (D.constructorVars info)
                 in addKindInfo' tyVars di <$> D.constructorFields info
  }


-- | Compute a prism's name by prefixing an underscore for normal
-- constructors and period for operators.
prismName :: Name -> Name
prismName n = case nameBase n of
                [] -> error "prismName: empty name base?"
                x:xs | isUpper x -> mkName ('_':x:xs)
                     | otherwise -> mkName ('.':x:xs) -- operator


-- | Quantify all the free variables in a type.
close :: Type -> Type
close (ForallT vars cx ty) = quantifyType vars cx ty
close ty                   = quantifyType []   [] ty
