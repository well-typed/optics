{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module MetaMetaPost where

import           Prelude ()
import           Prelude.Compat

import           Control.Applicative    (liftA2)
import           Control.Monad.State
import           Data.Foldable          (traverse_)
import           Data.Kind              (Type)
import           Data.List              (intercalate)
import           Generics.SOP
import           Generics.SOP.NP        (collapse_NP)
import           Generics.SOP.Optics    (productRep, rep, sop)
import qualified GHC.Generics           as GHC

import           Optics
import           Optics.State.Operators

import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Representable + generics-sop
-------------------------------------------------------------------------------

-- | A variant of 'Representable' with functional dependencies.
class Traversable f => Representable i f | f -> i where
    index :: i -> f a -> a
    default index :: (IsEnumType i, IsProductType (f a) fa, GIndex (Code i) fa a) => i -> f a -> a
    index i fa = gindex (view (rep % sop) i) (view productRep fa)

    tabulate :: (i -> a) -> f a
    default tabulate :: (IsEnumType i, IsProductType (f a) fa, GTabulate (Code i) fa a) => (i -> a) -> f a
    tabulate f = review productRep $ gtabulate (f . review (rep % sop))

    rix :: Eq i => i -> Lens' (f a) a
    rix ty = lens (index ty) $ \s x -> tabulate $ \ty' ->
        if ty == ty'
        then x
        else index ty' s


class GIndex (enum :: [[Type]]) (product :: [Type]) a where
    gindex :: NS (NP I) enum -> NP I product -> a

instance GIndex '[] '[] a where
    gindex ns _ = case ns of {}

instance (GIndex xs ys a, x ~ '[], a ~ a') => GIndex (x ': xs) (a' ': ys) a where
    gindex (Z Nil) (I x :* _)  = x
    gindex (S xs ) (_   :* ys) = gindex xs ys


class GTabulate (enum :: [[Type]]) (product :: [Type]) a where
    gtabulate :: (NS (NP I) enum -> a) -> NP I product

instance GTabulate '[] '[] a where
    gtabulate _ = Nil

instance (GTabulate xs ys a, x ~ '[], a ~ a') => GTabulate (x ': xs) (a' ': ys) a where
    gtabulate f = I (f $ Z Nil) :* gtabulate (f . S)


class GTraversableWithIndex (enum :: [[Type]]) (productA :: [Type]) (productB :: [Type]) a b where
    gitraverse' :: Applicative f => (NS (NP I) enum -> a -> f b) -> NP I productA -> f (NP I productB)

instance GTraversableWithIndex '[] '[] '[] a b where
    gitraverse' _ Nil = pure Nil

instance (GTraversableWithIndex is xs ys x y, i ~ '[], x ~ x', y ~ y') => GTraversableWithIndex (i ': is) (x ': xs) (y ': ys) x' y' where
    gitraverse' f (I x :* xs) = liftA2 (\y ys -> I y :* ys)
        (f (Z Nil) x)
        (gitraverse' (f . S) xs)

gitraverse :: (Applicative f, IsEnumType i, IsProductType s xs, IsProductType t ys, GTraversableWithIndex (Code i) xs ys a b) => (i -> a -> f b) -> s -> f t
gitraverse f xs = review productRep <$> gitraverse' (\i a -> f (review (rep % sop) i) a) (view productRep xs)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Ty
    = Numeric
    | Product
    | Path
    | Picture
    | Color
  deriving (Eq, Ord, GHC.Generic)

instance Generic Ty

data STy (ty :: Ty) where
    SNumeric :: STy 'Numeric
    SProduct :: STy 'Product
    SPath    :: STy 'Path
    SPicture :: STy 'Picture
    SColor   :: STy 'Color

class    ITy (ty :: Ty) where sty :: STy ty
instance ITy 'Numeric   where sty = SNumeric
instance ITy 'Product   where sty = SProduct
instance ITy 'Path      where sty = SPath
instance ITy 'Picture   where sty = SPicture
instance ITy 'Color     where sty = SColor

class INPTy (tys :: [Ty]) where
    npsty :: NP STy tys

instance INPTy '[] where npsty = Nil
instance (ITy ty, INPTy tys) => INPTy (ty ': tys) where npsty = sty :* npsty

reflect :: STy ty -> Ty
reflect SNumeric = Numeric
reflect SProduct = Product
reflect SPath    = Path
reflect SPicture = Picture
reflect SColor   = Color

data PerTy a = PerTy a a a a a
  deriving (Functor, Foldable, Traversable, GHC.Generic)

instance Generic (PerTy a)
instance Representable Ty PerTy

-------------------------------------------------------------------------------
-- Variable name
-------------------------------------------------------------------------------

data VarN s ty = VarN (STy ty) String

varPrefix :: Ty -> String
varPrefix Numeric = "n"
varPrefix Product = "w"
varPrefix Path    = "p"
varPrefix Picture = "q"
varPrefix Color   = "c"

varName :: Ty -> Int -> String
varName ty n = varPrefix ty ++ show n

-------------------------------------------------------------------------------
-- Macro name
-------------------------------------------------------------------------------

data MacroN s tys ty = MacroN (NP STy tys) (STy ty) String

-------------------------------------------------------------------------------
-- Paths
-------------------------------------------------------------------------------

-- TODO: no paths with cycles for now.

-- | Path piece
data PP s
    = PP (Expr s 'Product)
    | PPDir (Expr s 'Product) (Expr s 'Product)

-- | Path
data Path' s
    = PEnd (PP s)
    | PCons (PP s) (Path' s)


class PathCons a s where
    (...) :: a -> Path' s -> Path' s

instance PathCons (Expr s 'Product) s where
    e ... p = PCons (PP e) p

instance s ~ s' => PathCons (Expr s 'Product, Expr s' 'Numeric) s where
    (e, d) ... p = PCons (PPDir e (dir_ d)) p

infixr 4 ..., ....

(....) :: (PathCons a s, PathEnd b s) => a -> b -> Path' s
a .... b = a ... pathEnd b

class PathEnd a s where
    pathEnd :: a -> Path' s

instance PathEnd (Expr s 'Product) s where
    pathEnd = PEnd . PP

instance s ~ s' => PathEnd (Expr s 'Product, Expr s' 'Numeric) s where
    pathEnd (e, d) = PEnd (PPDir e (dir_ d))

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

data Expr s (ty :: Ty) where
    V        :: VarN s ty -> Expr s ty
    L        :: Double -> Expr s 'Numeric
    P        :: Path' s -> Expr s 'Path
    Pair     :: Expr s 'Numeric -> Expr s 'Numeric -> Expr s 'Product
    RGB      :: Expr s 'Numeric -> Expr s 'Numeric -> Expr s 'Numeric -> Expr s 'Color
    TheLabel :: String -> Expr s 'Product -> Expr s 'Picture

    PlusNumeric  :: Expr s 'Numeric -> Expr s 'Numeric -> Expr s 'Numeric
    MinusNumeric :: Expr s 'Numeric -> Expr s 'Numeric -> Expr s 'Numeric
    TimesNumeric :: Expr s 'Numeric -> Expr s 'Numeric -> Expr s 'Numeric

    PlusProduct  :: Expr s 'Product -> Expr s 'Product -> Expr s 'Product
    MinusProduct :: Expr s 'Product -> Expr s 'Product -> Expr s 'Product

    Subpath :: Expr s 'Product -> Expr s 'Path -> Expr s 'Path

    IntersectionPoint :: Expr s 'Path -> Expr s 'Path -> Expr s 'Product
    IntersectionTimes :: Expr s 'Path -> Expr s 'Path -> Expr s 'Product

    Call :: MacroN s tys ty -> NP (Expr s) tys -> Expr s ty

    Circle :: Expr s 'Numeric -> Expr s 'Product -> Expr s 'Path

-------------------------------------------------------------------------------
-- Macro
-------------------------------------------------------------------------------

newtype Macro s tys ty = Macro (forall z. Stmts z (Expr z ty))

macroArgs :: NP STy xs -> NP (VarN s) xs
macroArgs = go [ ['l', c] | c <- ['a' .. 'z'] ] where
    go :: [String] -> NP STy xs -> NP (VarN s) xs
    go _ Nil                = Nil
    go (n : ns) (ty :* tys) = VarN ty n :* go ns tys
    go [] _                 = error "too many arguments"

-------------------------------------------------------------------------------
-- Statements
-------------------------------------------------------------------------------

data Stmt s where
    Comment   :: String -> Stmt s

    Bind      :: VarN s ty -> Expr s ty -> Stmt s
    BindFst   :: VarN s 'Numeric -> Expr s 'Product -> Stmt s
    BindSnd   :: VarN s 'Numeric -> Expr s 'Product -> Stmt s

    VarDef    :: MacroN s tys ty -> Macro s tys ty -> Stmt s

    DrawPic   :: Expr s 'Picture -> Maybe (Expr s 'Color) -> Stmt s
    DrawPath  :: Expr s 'Path -> Maybe (Expr s 'Color) -> Stmt s
    DrawArrow :: Expr s 'Path -> Maybe (Expr s 'Color) -> Stmt s

    -- used in vardef
    Expr      :: Expr s ty -> Stmt s

data St s = St
    { stStmts :: [Stmt s] -> [Stmt s]
    , stVars  :: PerTy Int
    }

instance
  (k ~ A_Lens, s ~ s', a ~ ([Stmt s] -> [Stmt s]), a ~ b
  ) => LabelOptic "stmts" k (St s) (St s') a b where
    labelOptic = lens stStmts $ \s x -> s { stStmts = x }

instance
  (k ~ A_Lens, s ~ s', a ~ PerTy Int, a ~ b
  ) => LabelOptic "vars" k (St s) (St s') a b where
    labelOptic = lens stVars $ \s x -> s { stVars = x }

emptyS :: St s
emptyS = St
    { stStmts = id
    , stVars  = tabulate (const 0)
    }

newtype Stmts s a = Stmts (State (St s) a)
    deriving (Functor, Applicative, Monad, MonadState (St s))

-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------

class Output a where
    output :: a -> ShowS

output' :: Output a => a -> String
output' x = output x ""

instance Output Char where
    output = showChar

instance a ~ Char => Output [a] where
    output = showString

instance Output (VarN s ty) where
    output (VarN _ s) = output s

instance Output (PP s) where
    output (PP p)      = output p
    output (PPDir p d) = output p . output '{' . output d . output '}'

instance Output (Path' s) where
    output (PEnd pp)   = output pp
    output (PCons h t) = output h . output ".." . output t

instance Output (Expr s ty) where
    output (V v) = output v
    output (L n) = showParen (n < 0) $ showsPrec 11 n
    output (P p) = showParen True (output p)
    output (Pair x y) = showParen True
        $ output x
        . output ", "
        . output y
    output (RGB x y z) = showParen True
        $ output x
        . output ", "
        . output y
        . output ", "
        . output z
    output (TheLabel str e)
        = output "thelabel(btex $"
        . output str
        . output "$ etex, "
        . output e
        . output ")"

    output (PlusNumeric x y) = showParen True
        $ output x
        . output " + "
        . output y
    output (MinusNumeric x y) = showParen True
        $ output x
        . output " - "
        . output y
    output (TimesNumeric x y) = showParen True
        $ output x
        . output " * "
        . output y

    output (PlusProduct x y) = showParen True
        $ output x
        . output " + "
        . output y
    output (MinusProduct x y) = showParen True
        $ output x
        . output " - "
        . output y

    output (Subpath a p) = output "subpath " . output a . output " of "  . output p

    output (IntersectionPoint a b) = showParen True
        $ output a
        . output " intersectionpoint "
        . output b

    output (IntersectionTimes a b) = showParen True
        $ output a
        . output " intersectiontimes "
        . output b

    output (Call (MacroN _ _ n) args)
        = output n
        . output "("
        . showString (intercalate ", " $ collapse_NP $ map_NP (K . output') $ args)
        . output ")"

    output (Circle scale shft) = showParen True
        $ output "fullcircle scaled "
        . output scale
        . output " shifted "
        . output shft

instance Output (Stmt s) where
    output (Comment str) = output "% " . output str
    output (Bind v e)    = output v . output " = " . output e . output ";"
    output (BindFst v e) = output "(" . output v . output ", whatever) = " . output e . output ";"
    output (BindSnd v e) = output "(whatever, " . output v . output ") = " . output e . output ";"

    output (VarDef (MacroN tys _ name) (Macro body))
        = output "vardef " . output name . output "(expr " . arguments. output ") = save n, w, p, q, c;\n"
        . foldr (\a b -> showString "  " . output a . showChar '\n' . b) (output "enddef;") (runStmts $ body >>= putStmt . Expr)
      where
        arguments
            = showString
            $ intercalate ", "
            $ collapse_NP
            $ map_NP (\(VarN _ n) -> K n)
            $ macroArgs tys

    output (DrawPath e c)
        = output "draw ("
        . output e
        . output ")"
        . maybe id (\x -> output " withcolor " . output x) c
        . output ";"

    output (DrawPic e c)
        = output "draw ("
        . output e
        . output ")"
        . maybe id (\x -> output " withcolor " . output x) c
        . output ";"

    output (DrawArrow e c)
        = output "drawarrow ("
        . output e
        . output ")"
        . maybe id (\x -> output " withcolor " . output x) c
        . output ";"

    output (Expr e) = output e -- note: no trailing ;

-- own map_NP without SListI requirement
map_NP :: (forall (x :: Ty). f x -> g x) -> NP f xs -> NP g xs
map_NP _ Nil       = Nil
map_NP f (x :* xs) = f x :* map_NP f xs

-------------------------------------------------------------------------------
-- DSL: Expr
-------------------------------------------------------------------------------

instance (FromInteger a, Additive a a a, Mult a a a) => Num (Expr s a) where
    fromInteger = fromInteger_
    (+) = (.+)
    (-) = (.-)
    (*) = (.*)

    abs    = error "abs @Expr"
    signum = error "signum @Expr"

infixl 6 .+, .-
infixl 7 .*

class FromInteger a where
    fromInteger_ :: Integer -> Expr s a

instance FromInteger 'Numeric where
    fromInteger_ = L . fromInteger


class Additive a b c | a b -> c where
    (.+) :: Expr s a -> Expr s b -> Expr s c
    (.-) :: Expr s a -> Expr s b -> Expr s c
    negate_ :: (FromInteger a, a ~ b) => Expr s b -> Expr s c

instance Additive 'Numeric 'Numeric 'Numeric where
    (.+) = PlusNumeric
    (.-) = MinusNumeric

    negate_ (L n) = L (negate n)
    negate_ n     = L 0 .- n

instance Additive 'Product 'Product 'Product where
    (.+) = PlusProduct
    (.-) = MinusProduct

    negate_ n = Pair (L 0) (L 0) .- n


class Mult a b c | a b -> c where
    (.*) :: Expr s a -> Expr s b -> Expr s c

instance Mult 'Numeric 'Numeric 'Numeric where
    (.*) = TimesNumeric


-------------------------------------------------------------------------------
-- DSL: functions
-------------------------------------------------------------------------------

class    Bbox ty       where bbox_ :: Expr s ty -> Expr s 'Path
instance Bbox 'Path    where bbox_ = call1 (MacroN npsty sty "bbox")
instance Bbox 'Picture where bbox_ = call1 (MacroN npsty sty "bbox")

reverse_ :: Expr s 'Path -> Expr s 'Path
reverse_ = call1 (MacroN npsty sty "reverse")

length_ :: Expr s 'Path -> Expr s 'Numeric
length_ = call1 (MacroN npsty sty "length")

dir_ :: Expr s 'Numeric -> Expr s 'Product
dir_ = call1 (MacroN npsty sty "dir")

-------------------------------------------------------------------------------
-- DSL: Subpath
-------------------------------------------------------------------------------

-- TODO: make general "ToProduct"
class Subpath a s where
    subpath_ :: a -> Expr s 'Path -> Expr s 'Path

instance s ~ s' => Subpath (Expr s' 'Product) s where
    subpath_ = Subpath

instance (s ~ s1, s ~ s2) => Subpath (Expr s1 'Numeric, Expr s2 'Numeric) s where
    subpath_ (x, y) = subpath_ (Pair x y)

-------------------------------------------------------------------------------
-- DSL: Stmt
-------------------------------------------------------------------------------

putStmt :: Stmt s -> Stmts s ()
putStmt stmt = #stmts %= \ss -> ss . (stmt :)

bind_ :: forall ty s. ITy ty => Expr s ty -> Stmts s (Expr s ty)
bind_ = bindImpl Bind

bindFst_ :: Expr s 'Product -> Stmts s (Expr s 'Numeric)
bindFst_ = bindImpl BindFst

bindSnd_ :: Expr s 'Product -> Stmts s (Expr s 'Numeric)
bindSnd_ = bindImpl BindSnd

bindImpl :: forall ty ty' s. ITy ty => (VarN s ty -> Expr s ty' -> Stmt s) -> Expr s ty' -> Stmts s (Expr s ty)
bindImpl bind' expr = do
    let sty' = sty :: STy ty
    let ty   = reflect sty'
    idx <- #vars % rix ty <<%= succ

    let var = VarN sty' (varName ty idx)
    let stmt = bind' var expr :: Stmt s
    putStmt stmt

    return (V var)

class    Draw ty       where
    draw_  :: Expr s ty -> Stmts s ()
    drawC_ :: Expr s 'Color -> Expr s ty -> Stmts s ()

instance Draw 'Picture where
    draw_ expr      = putStmt $ DrawPic expr Nothing
    drawC_ clr expr = putStmt (DrawPic expr (Just clr))

instance Draw 'Path    where
    draw_ expr      = putStmt $ DrawPath expr Nothing
    drawC_ clr expr = putStmt (DrawPath expr (Just clr))

drawarrow_ :: Expr s 'Path -> Stmts s ()
drawarrow_ expr = putStmt (DrawArrow expr Nothing)

drawarrowC_ :: Expr s 'Color -> Expr s 'Path -> Stmts s ()
drawarrowC_ clr expr = putStmt (DrawArrow expr (Just clr))

-------------------------------------------------------------------------------
-- DSL: VarDef
-------------------------------------------------------------------------------

vardefNP
    :: ITy ty
    => String
    -> NP STy tys
    -> (forall z. NP (Expr z) tys -> Stmts z (Expr z ty))
    -> Stmts s (MacroN s tys ty)
vardefNP name tys f = do
    let mn = MacroN tys sty name
    putStmt $ VarDef mn $ Macro $ f $ map_NP V $ macroArgs tys
    return mn

vardef1
    :: ITy ty
    => String
    -> STy ty1
    -> (forall z. Expr z ty1 -> Stmts z (Expr z ty))
    -> Stmts s (Expr s ty1 -> Expr s ty)
vardef1 name ty1 f = do
    m <- vardefNP name (ty1 :* Nil) $ \(x :* Nil) -> f x
    return (call1 m)

vardef2
    :: ITy ty
    => String
    -> STy ty1 -> STy ty2
    -> (forall z. Expr z ty1 -> Expr z ty2 -> Stmts z (Expr z ty))
    -> Stmts s (Expr s ty1 -> Expr s ty2 -> Expr s ty)
vardef2 name ty1 ty2 f = do
    m <- vardefNP name (ty1 :* ty2 :* Nil) $ \(x :* y :* Nil) -> f x y
    return (call2 m)

vardef3
    :: ITy ty
    => String
    -> STy ty1 -> STy ty2 -> STy ty3
    -> (forall z. Expr z ty1 -> Expr z ty2 -> Expr z ty3 -> Stmts z (Expr z ty))
    -> Stmts s (Expr s ty1 -> Expr s ty2 -> Expr s ty3 -> Expr s ty)
vardef3 name ty1 ty2 ty3 f = do
    m <- vardefNP name (ty1 :* ty2 :* ty3 :* Nil) $ \(x :* y :* z :* Nil) -> f x y z
    return (call3 m)

-------------------------------------------------------------------------------
-- DSL: Call
-------------------------------------------------------------------------------

call1 :: MacroN s '[ty1] ty -> Expr s ty1 -> Expr s ty
call1 m x1 = Call m (x1 :* Nil)

call2 :: MacroN s '[ty1, ty2] ty -> Expr s ty1 -> Expr s ty2 -> Expr s ty
call2 m x1 x2 = Call m (x1 :* x2 :* Nil)

call3 :: MacroN s '[ty1, ty2, ty3] ty -> Expr s ty1 -> Expr s ty2 -> Expr s ty3 -> Expr s ty
call3 m x1 x2 x3 = Call m (x1 :* x2 :* x3 :* Nil)

-------------------------------------------------------------------------------
-- optimise
-------------------------------------------------------------------------------

varsExpr :: Expr s ty -> Set.Set String
varsExpr (V (VarN _ s))          = Set.singleton s
varsExpr (L _)                   = Set.empty
varsExpr (P p)                   = varsPath p
varsExpr (Pair x y)              = varsExpr x <> varsExpr y
varsExpr (RGB r g b)             = varsExpr r <> varsExpr g <> varsExpr b
varsExpr (TheLabel _ p)          = varsExpr p
varsExpr (PlusNumeric x y)       = varsExpr x <> varsExpr y
varsExpr (MinusNumeric x y)      = varsExpr x <> varsExpr y
varsExpr (TimesNumeric x y)      = varsExpr x <> varsExpr y
varsExpr (PlusProduct x y)       = varsExpr x <> varsExpr y
varsExpr (MinusProduct x y)      = varsExpr x <> varsExpr y
varsExpr (Subpath x y)           = varsExpr x <> varsExpr y
varsExpr (IntersectionPoint x y) = varsExpr x <> varsExpr y
varsExpr (IntersectionTimes x y) = varsExpr x <> varsExpr y
varsExpr (Call _ xs)             = Set.unions $ collapse_NP $ map_NP (K . varsExpr) xs
varsExpr (Circle x y)            = varsExpr x <> varsExpr y

varsPath :: Path' s -> Set.Set String
varsPath (PEnd pp)    = varsPP pp
varsPath (PCons pp p) = varsPP pp <> varsPath p

varsPP :: PP s -> Set.Set String
varsPP (PP e)      = varsExpr e
varsPP (PPDir p d) = varsExpr p <> varsExpr d

varsStmt :: Stmt s -> Set.Set String
varsStmt (Comment _)     = Set.empty
varsStmt (Bind _ e)      = varsExpr e
varsStmt (BindFst _ e)   = varsExpr e
varsStmt (BindSnd _ e)   = varsExpr e
varsStmt (VarDef _ _)    = Set.empty
varsStmt (DrawPic p c)   = varsExpr p <> maybe Set.empty varsExpr c
varsStmt (DrawPath p c)  = varsExpr p <> maybe Set.empty varsExpr c
varsStmt (DrawArrow p c) = varsExpr p <> maybe Set.empty varsExpr c
varsStmt (Expr e)        = varsExpr e

optimise :: [Stmt s] -> [Stmt s]
optimise = snd . foldr f (Set.empty, []) where
    f :: Stmt s -> (Set.Set String, [Stmt s]) -> (Set.Set String, [Stmt s])
    f (Bind (VarN _ v) _) (used, ss)
        | v `Set.notMember` used = (used, ss)
    f (BindFst (VarN _ v) _) (used, ss)
        | v `Set.notMember` used = (used, ss)
    f (BindSnd (VarN _ v) _) (used, ss)
        | v `Set.notMember` used = (used, ss)
    f s (used, ss) = (varsStmt s <> used, s : ss)


-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

runStmts :: (forall s. Stmts s ()) -> [String]
runStmts m = case m of
    Stmts m' -> case execState m' emptyS of
        St ss _ -> map output' $ optimise $ ss []

printDiagram :: (forall s. Stmts s ()) -> IO ()
printDiagram d = do
    putStrLn "prologues := 3;"
    putStrLn "bboxmargin := 4bp;"

    -- latex prelude
    traverse_ putStrLn
        [ "verbatimtex"
        , "%&latex"
        , "\\documentclass{article}"
        , "\\begin{document}"
        , "etex"
        ]

    putStrLn "beginfig(1);"

    putStrLn "numeric n[];"
    putStrLn "pair w[];"
    putStrLn "picture q[];"
    putStrLn "path p[];"
    putStrLn "color c[];"

    putStrLn "picture diag;"
    putStrLn "diag := image("
    traverse_ putStrLn (runStmts d)
    putStrLn ");"
    putStrLn "picture diah;"
    -- not sure why we have to move image?
    -- horizonttal offset can be whatever
    putStrLn "diah = diag shifted (-(llcorner(diag)));"
    putStrLn "unfill bbox(diah scaled 2);"
    putStrLn "draw diah scaled 2;"

    putStrLn "endfig;"

    -- latex post
    traverse_ putStrLn
        [ "verbatimtex"
        , "\\end{document}"
        , "etex"
        ]

    putStrLn "end"
