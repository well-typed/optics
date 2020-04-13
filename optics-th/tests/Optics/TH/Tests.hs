{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Data.Functor.Const
import Data.Kind (Type)
import Data.Tagged
import Data.Typeable

import Optics.Core
import Optics.TH
import Optics.TH.Tests.T799 ()

data Pair a b = Pair a b
makePrisms ''Pair
makePrismLabels ''Pair

checkPair :: Iso (Pair a b) (Pair a' b') (a, b) (a', b')
checkPair = _Pair

checkPair_ :: Iso (Pair a b) (Pair a' b') (a, b) (a', b')
checkPair_ = #_Pair

data Sum a b = SLeft a | SRight b | SWeird Int
makePrisms ''Sum
makePrismLabels ''Sum

checkSLeft :: Prism (Sum a c) (Sum b c) a b
checkSLeft = _SLeft

checkSLeft_ :: Prism (Sum a c) (Sum b c) a b
checkSLeft_ = #_SLeft

checkSRight :: Prism (Sum c a) (Sum c b) a b
checkSRight = _SRight

checkSRight_ :: Prism (Sum c a) (Sum c b) a b
checkSRight_ = #_SRight

checkSWeird :: Prism' (Sum a b) Int
checkSWeird = _SWeird

checkSWeird_ :: Prism' (Sum a b) Int
checkSWeird_ = #_SWeird

data PairEq a b c where
  PairEq :: (Eq a, Eq b) => a -> b -> PairEq a b c
makePrisms ''PairEq
makePrismLabels ''PairEq

checkPairEq
  :: (Eq a', Eq b')
  => Iso (PairEq a b c) (PairEq a' b' c') (a, b) (a', b')
checkPairEq = _PairEq

checkPairEq_
  :: (Eq a', Eq b')
  => Iso (PairEq a b c) (PairEq a' b' c) (a, b) (a', b')
checkPairEq_ = #_PairEq

data Brr a where
  BrrA :: a -> Brr a
  BrrInt :: Int -> Brr Int
makePrisms ''Brr
makePrismLabels ''Brr

checkBrrA :: Prism' (Brr a) a
checkBrrA = _BrrA

checkBrrA_ :: Prism' (Brr a) a
checkBrrA_ = #_BrrA

checkBrrInt :: Prism' (Brr Int) Int
checkBrrInt = _BrrInt

checkBrrInt_ :: Prism' (Brr Int) Int
checkBrrInt_ = #_BrrInt

data Bzzt a b c where
  BzztShow :: Show a => a -> Bzzt a b c
  BzztRead :: Read b => b -> Bzzt a b c
makePrisms ''Bzzt
makePrismLabels ''Bzzt

checkBzztShow :: Show a => Prism (Bzzt a b c) (Bzzt a b c') a a
checkBzztShow = _BzztShow

-- We can't change b because of LabelOptic fundeps.
checkBzztShow_ :: Show a => Prism' (Bzzt a b c) a
checkBzztShow_ = #_BzztShow

checkBzztRead :: Read b => Prism (Bzzt a b c) (Bzzt a b c') b b
checkBzztRead = _BzztRead

-- We can't change b because of LabelOptic fundeps.
checkBzztRead_ :: Read b => Prism' (Bzzt a b c) b
checkBzztRead_ = #_BzztRead

data FooX a where
  FooX1, FooX2 :: { fooX_, fooY_ :: Int } -> FooX a
makePrisms ''FooX
makePrismLabels ''FooX

checkFooX1 :: Prism (FooX a) (FooX b) (Int, Int) (Int, Int)
checkFooX1 = _FooX1

-- We can't change a because of LabelOptic fundeps.
checkFooX1_ :: Prism' (FooX a) (Int, Int)
checkFooX1_ = #_FooX1

checkFooX2 :: Prism (FooX a) (FooX b) (Int, Int) (Int, Int)
checkFooX2 = _FooX2

-- We can't change a because of LabelOptic fundeps.
checkFooX2_ :: Prism' (FooX a) (Int, Int)
checkFooX2_ = #_FooX2

data ClassyTest = ClassyT1 Int | ClassyT2 String | ClassyT3 Char
makeClassyPrisms ''ClassyTest

checkClassyTest :: AsClassyTest r => Prism' r ClassyTest
checkClassyTest = _ClassyTest

checkClassyT1 :: AsClassyTest r => Prism' r Int
checkClassyT1 = _ClassyT1

checkClassyT2 :: AsClassyTest r => Prism' r String
checkClassyT2 = _ClassyT2

checkClassyT3 :: AsClassyTest r => Prism' r Char
checkClassyT3 = _ClassyT3

data Weird1 (a :: k -> Type) (b :: k -> Type) = Weird1
data WeirdThing a b = WeirdThing (Weird1 a (Const b))
makePrisms ''WeirdThing

--data FooZ (a :: k -> Type) (b :: k -> Type) where
--  FooZ :: FooZ (a :: Type -> Type) b
--makePrisms ''FooZ

----------------------------------------

data Bar a b c = Bar { _baz :: (a, b) }
makeLenses ''Bar
makeFieldLabelsWith lensRules ''Bar

checkBaz :: Iso (Bar a b c) (Bar a' b' c') (a, b) (a', b')
checkBaz = baz

-- We can't change c because of LabelOptic fundeps.
checkBaz_ :: Iso (Bar a b c) (Bar a' b' c) (a, b) (a', b')
checkBaz_ = #baz

data Quux a b = Quux { _quaffle :: Int, _quartz :: Double }
makeLenses ''Quux
makeFieldLabelsWith lensRules ''Quux

checkQuaffle :: Lens (Quux a b) (Quux a' b') Int Int
checkQuaffle = quaffle

-- We can't change a and b because of LabelOptic fundeps.
checkQuaffle_ :: Lens (Quux a b) (Quux a b) Int Int
checkQuaffle_ = #quaffle

checkQuartz :: Lens (Quux a b) (Quux a' b') Double Double
checkQuartz = quartz

-- We can't change a and b because of LabelOptic fundeps.
checkQuartz_ :: Lens (Quux a b) (Quux a b) Double Double
checkQuartz_ = #quartz

data Quark a = Qualified   { _gaffer :: a }
             | Unqualified { _gaffer :: a, _tape :: a }
makeLenses ''Quark
makeFieldLabelsWith lensRules ''Quark

checkGaffer :: Lens' (Quark a) a
checkGaffer = gaffer

checkGaffer_ :: Lens' (Quark a) a
checkGaffer_ = #gaffer

checkTape :: AffineTraversal' (Quark a) a
checkTape = tape

checkTape_ :: AffineTraversal' (Quark a) a
checkTape_ = #tape

data Hadron a b = Science { _a1 :: a, _a2 :: a, _c :: Either b [b] }
makeLenses ''Hadron
makeFieldLabelsWith lensRules ''Hadron

checkA1 :: Lens' (Hadron a b) a
checkA1 = a1

checkA1_ :: Lens' (Hadron a b) a
checkA1_ = #a1

checkA2 :: Lens' (Hadron a b) a
checkA2 = a2

checkA2_ :: Lens' (Hadron a b) a
checkA2_ = #a2

checkC :: Lens (Hadron a b) (Hadron a b') (Either b [b]) (Either b' [b'])
checkC = c

checkC_ :: Lens (Hadron a b) (Hadron a b') (Either b [b]) (Either b' [b'])
checkC_ = #c

data Perambulation a b
  = Mountains { _terrain    :: a
              , _altitude   :: b
              -- Having Eq here doesn't work with old unification logic because
              -- it was incomplete (and didn't seem to do anything).
              , _absurdity1 :: forall x y. Eq x => x -> y
              , _absurdity2 :: forall x y. Eq x => x -> y
              }
  | Beaches   { _terrain    :: a
              , _dunes      :: a
              , _absurdity1 :: forall x y. Eq x => x -> y
              }
makeLenses ''Perambulation
makeFieldLabelsWith lensRules ''Perambulation

checkTerrain :: Lens' (Perambulation a b) a
checkTerrain = terrain

checkTerrain_ :: Lens' (Perambulation a b) a
checkTerrain_ = #terrain

checkAltitude :: AffineTraversal (Perambulation a b) (Perambulation a b') b b'
checkAltitude = altitude

checkAltitude_ :: AffineTraversal (Perambulation a b) (Perambulation a b') b b'
checkAltitude_ = #altitude

checkAbsurdity1 :: Eq x => Getter (Perambulation a b) (x -> y)
checkAbsurdity1 = absurdity1

checkAbsurdity2 :: Eq x => AffineFold (Perambulation a b) (x -> y)
checkAbsurdity2 = absurdity2

checkDunes :: AffineTraversal' (Perambulation a b) a
checkDunes = dunes

checkDunes_ :: AffineTraversal' (Perambulation a b) a
checkDunes_ = #dunes

makeLensesFor [ ("_terrain", "allTerrain")
              , ("_dunes", "allTerrain")
              , ("_absurdity1", "absurdities")
              , ("_absurdity2", "absurdities")
              ] ''Perambulation

makeFieldLabelsFor [ ("_terrain", "allTerrain")
                   , ("_dunes", "allTerrain")
                   ] ''Perambulation

checkAllTerrain :: Traversal (Perambulation a b) (Perambulation a' b) a a'
checkAllTerrain = allTerrain

checkAllTerrain_ :: Traversal (Perambulation a b) (Perambulation a' b) a a'
checkAllTerrain_ = #allTerrain

checkAbsurdities :: Eq x => Fold (Perambulation a b) (x -> y)
checkAbsurdities = absurdities

data LensCrafted a = Still { _still :: a }
                   | Works { _still :: a }
makeLenses ''LensCrafted
makeFieldLabelsWith lensRules ''LensCrafted

checkStill :: Lens (LensCrafted a) (LensCrafted b) a b
checkStill = still

checkStill_ :: Lens (LensCrafted a) (LensCrafted b) a b
checkStill_ = #still

data Task a = Task
  { taskOutput :: a -> IO ()
  , taskState :: a
  , taskStop :: IO ()
  }

makeLensesFor [ ("taskOutput", "outputLens")
              , ("taskState", "stateLens")
              , ("taskStop", "stopLens")
              ] ''Task

makeFieldLabelsFor [ ("taskOutput", "output")
                   , ("taskState", "state")
                   , ("taskStop", "stop")
                   ] ''Task

checkOutputLens :: Lens' (Task a) (a -> IO ())
checkOutputLens = outputLens

checkOutput_ :: Lens' (Task a) (a -> IO ())
checkOutput_ = #output

checkStateLens :: Lens' (Task a) a
checkStateLens = stateLens

checkState_ :: Lens' (Task a) a
checkState_ = #state

checkStopLens :: Lens' (Task a) (IO ())
checkStopLens = stopLens

checkStop_ :: Lens' (Task a) (IO ())
checkStop_ = #stop

data Mono a = Mono { _monoFoo :: a, _monoBar :: Int }
makeClassy ''Mono
-- class HasMono t where
--   mono :: Simple Lens t Mono
-- instance HasMono Mono where
--   mono = id

checkMono :: HasMono t a => Lens' t (Mono a)
checkMono = mono

checkMono' :: Lens' (Mono a) (Mono a)
checkMono' = mono

checkMonoFoo :: HasMono t a => Lens' t a
checkMonoFoo = monoFoo

checkMonoBar :: HasMono t a => Lens' t Int
checkMonoBar = monoBar

data Nucleosis = Nucleosis { _nuclear :: Mono Int }
makeClassy ''Nucleosis
-- class HasNucleosis t where
--   nucleosis :: Simple Lens t Nucleosis
-- instance HasNucleosis Nucleosis

checkNucleosis :: HasNucleosis t => Lens' t Nucleosis
checkNucleosis = nucleosis

checkNucleosis' :: Lens' Nucleosis Nucleosis
checkNucleosis' = nucleosis

checkNuclear :: HasNucleosis t => Lens' t (Mono Int)
checkNuclear = nuclear

instance HasMono Nucleosis Int where
  mono = nuclear

-- Dodek's example
data Foo = Foo { _fooX, _fooY :: Int }
makeClassy ''Foo
makeFieldLabels ''Foo

checkFoo :: HasFoo t => Lens' t Foo
checkFoo = foo

checkFoo' :: Lens' Foo Foo
checkFoo' = foo

checkFooX :: HasFoo t => Lens' t Int
checkFooX = fooX

checkFooX_ :: Lens' Foo Int
checkFooX_ = #x

checkFooY :: HasFoo t => Lens' t Int
checkFooY = fooY

checkFooY_ :: Lens' Foo Int
checkFooY_ = #y

data Dude a = Dude
    { dudeLevel        :: Int
    , dudeAlias        :: String
    , dudeLife         :: ()
    , dudeThing        :: a
    }
makeFields ''Dude
makeFieldLabels ''Dude

checkLevel :: HasLevel t a => Lens' t a
checkLevel = level

checkLevel' :: Lens' (Dude a) Int
checkLevel' = level

checkLevel_ :: Lens' (Dude a) Int
checkLevel_ = #level

checkAlias :: HasAlias t a => Lens' t a
checkAlias = alias

checkAlias' :: Lens' (Dude a) String
checkAlias' = alias

checkAlias_ :: Lens' (Dude a) String
checkAlias_ = #alias

checkLife :: HasLife t a => Lens' t a
checkLife = life

checkLife' :: Lens' (Dude a) ()
checkLife' = life

checkLife_ :: Lens' (Dude a) ()
checkLife_ = #life

checkThing :: HasThing t a => Lens' t a
checkThing = thing

checkThing' :: Lens' (Dude a) a
checkThing' = thing

checkThing_ :: Lens (Dude a) (Dude b) a b
checkThing_ = #thing

data Lebowski a = Lebowski
    { _lebowskiAlias    :: String
    , _lebowskiLife     :: Int
    , _lebowskiMansion  :: String
    , _lebowskiThing    :: Maybe a
    }
makeFields ''Lebowski
makeFieldLabels ''Lebowski

checkAlias2 :: Lens' (Lebowski a) String
checkAlias2 = alias

checkAlias2_ :: Lens' (Lebowski a) String
checkAlias2_ = #alias

checkLife2 :: Lens' (Lebowski a) Int
checkLife2 = life

checkLife2_ :: Lens' (Lebowski a) Int
checkLife2_ = #life

checkMansion :: HasMansion t a => Lens' t a
checkMansion = mansion

checkMansion' :: Lens' (Lebowski a) String
checkMansion' = mansion

checkMansion_ :: Lens' (Lebowski a) String
checkMansion_ = #mansion

checkThing2 :: Lens' (Lebowski a) (Maybe a)
checkThing2 = thing

checkThing2_ :: Lens (Lebowski a) (Lebowski b) (Maybe a) (Maybe b)
checkThing2_ = #thing

data Kinded1 (a :: k1) (b :: k2) = Kinded
  { _kinded1Thing :: Tagged '(a, b) Int
  }
makeFieldLabels ''Kinded1

checkKinded1Thing :: Iso (Kinded1 (a  :: k1 ) (b  :: k2 ))
                         (Kinded1 (a' :: k1') (b' :: k2'))
                         (Tagged '(a , b ) Int)
                         (Tagged '(a', b') Int)
checkKinded1Thing = #thing

data Kinded2 k a = Kinded2
  { _kinded2Thing :: Proxy (a :: k)
  }
makeFieldLabels ''Kinded2

checkKinded2Thing :: Iso (Kinded2 k  a )
                         (Kinded2 k' a')
                         (Proxy (a  :: k ))
                         (Proxy (a' :: k'))
checkKinded2Thing = #thing

type family Fam (a :: k)
type instance Fam Int = String

-- unambiguous type family application
data FamRec1 a = FamRec1 { _famRec1Thing :: a -> Fam a }
makeFieldLabels ''FamRec1

checkFamRec1Thing :: Iso (FamRec1 a) (FamRec1 b) (a -> Fam a) (b -> Fam b)
checkFamRec1Thing = #thing

type family FamInj1 (a :: k) b = r | r -> a

-- type family injective in its first parameter
data FamRec2 a b = FamRec2 { _famRec2Thing :: FamInj1 a b }
makeFieldLabels ''FamRec2

checkFamRec2Thing :: Iso (FamRec2 a b) (FamRec2 a' b) (FamInj1 a b) (FamInj1 a' b)
checkFamRec2Thing = #thing

type family a :#: b = r | r -> b

-- infix type family injective in its second parameter
data FamRec3 a b = FamRec3 { _famRec3Thing :: a :#: b }
makeFieldLabels ''FamRec3

checkFamRec3Thing :: Iso (FamRec3 a b) (FamRec3 a b') (a :#: b) (a :#: b')
checkFamRec3Thing = #thing

-- ambiguous type family application, type-preserving optic
data FamRec4 a = FamRec4 { _famRec4Thing :: FamInj1 (Fam a) a }
makeFieldLabels ''FamRec4 -- no error

-- no type changing optic here
checkFamRec4Thing :: Iso' (FamRec4 a) (FamInj1 (Fam a) a)
checkFamRec4Thing = #thing

type family FamInj2 a b (c :: k) = r | r -> a b c

-- poly kinded shenenigans
data FamRec5 a b (c :: k) = FamRec5 { _famRec5Thing :: FamInj2 a b '[c] }
makeFieldLabels ''FamRec5

-- type-changing, kind-changing optic
checkFamRec5Thing :: Iso (FamRec5 a  b  (c  :: k ))
                         (FamRec5 a' b' (c' :: k'))
                         (FamInj2 a  b  '[c ])
                         (FamInj2 a' b' '[c'])
checkFamRec5Thing = #thing

-- ambiguous type family application + Tagged = type-changing optic
data FamRec6 a = FamRec6 { _famRec6Thing :: Tagged a (Fam a) }
makeFieldLabels ''FamRec6

checkFamRec6Thing
  :: Iso (FamRec6 a) (FamRec6 b) (Tagged a (Fam a)) (Tagged b (Fam b))
checkFamRec6Thing = #thing

-- nested injective type family application + kind polymorphism
data FamRec7 a b (c :: [k]) = FamRec7
  { _famRec7Thing :: FamInj1 (b :#: (a -> FamInj1 c b)) b
  }
makeFieldLabels ''FamRec7

checkFamRec7Thing :: Iso (FamRec7 a b  (c  :: [k ]))
                         (FamRec7 a' b (c' :: [k']))
                         (FamInj1 (b :#: (a -> FamInj1 c b)) b)
                         (FamInj1 (b :#: (a' -> FamInj1 c' b)) b)
checkFamRec7Thing = #thing

data FamRec a = FamRec
  { _famRecThing :: Fam a
  , _famRecUniqueToFamRec :: Fam a
  }
makeFields ''FamRec
makeFieldLabels ''FamRec

checkFamRecThing :: Lens' (FamRec a) (Fam a)
checkFamRecThing = thing

checkFamRecThing_ :: Lens' (FamRec a) (Fam a)
checkFamRecThing_ = #thing

checkFamRecUniqueToFamRec :: Lens' (FamRec a) (Fam a)
checkFamRecUniqueToFamRec = uniqueToFamRec

checkFamRecUniqueToFamRec_ :: Lens' (FamRec a) (Fam a)
checkFamRecUniqueToFamRec_ = #uniqueToFamRec

checkFamRecView :: FamRec Int -> String
checkFamRecView = view thing

checkFamRecView_ :: FamRec Int -> String
checkFamRecView_ = view #thing

data AbideConfiguration a = AbideConfiguration
    { _acLocation       :: String
    , _acDuration       :: Int
    , _acThing          :: a
    }
makeLensesWith abbreviatedFields ''AbideConfiguration
makeFieldLabelsWith abbreviatedFieldLabels ''AbideConfiguration

checkLocation :: HasLocation t a => Lens' t a
checkLocation = location

checkLocation' :: Lens' (AbideConfiguration a) String
checkLocation' = location

checkLocation_ :: Lens' (AbideConfiguration a) String
checkLocation_ = #location

checkDuration :: HasDuration t a => Lens' t a
checkDuration = duration

checkDuration' :: Lens' (AbideConfiguration a) Int
checkDuration' = duration

checkDuration_ :: Lens' (AbideConfiguration a) Int
checkDuration_ = #duration

checkThing3 :: Lens' (AbideConfiguration a) a
checkThing3 = thing

checkThing3_ :: Lens (AbideConfiguration a) (AbideConfiguration b) a b
checkThing3_ = #thing

dudeDrink :: String
dudeDrink      = (Dude 9 "El Duderino" () "white russian")      ^. thing
lebowskiCarpet :: Maybe String
lebowskiCarpet = (Lebowski "Mr. Lebowski" 0 "" (Just "carpet")) ^. thing
abideAnnoyance :: String
abideAnnoyance = (AbideConfiguration "the tree" 10 "the wind")  ^. thing

declareLenses [d|
  data Quark1 a = Qualified1   { gaffer1 :: a }
                | Unqualified1 { gaffer1 :: a, tape1 :: a }
  |]
-- data Quark1 a = Qualified1 a | Unqualified1 a a

checkGaffer1 :: Lens' (Quark1 a) a
checkGaffer1 = gaffer1

checkTape1 :: AffineTraversal' (Quark1 a) a
checkTape1 = tape1

declareFieldLabels [d|
  data Quark2 a = Qualified2   { gaffer2 :: a }
                | Unqualified2 { gaffer2 :: a, tape2 :: a }
  |]

checkGaffer2 :: Lens' (Quark2 a) a
checkGaffer2 = #gaffer2

checkTape2 :: AffineTraversal' (Quark2 a) a
checkTape2 = #tape2

declarePrisms [d|
  data Exp = Lit Int | Var String | Lambda { bound::String, body::Exp }
  |]
-- data Exp = Lit Int | Var String | Lambda { bound::String, body::Exp }

checkLit :: Int -> Exp
checkLit = Lit

checkVar :: String -> Exp
checkVar = Var

checkLambda :: String -> Exp -> Exp
checkLambda = Lambda

check_Lit :: Prism' Exp Int
check_Lit = _Lit

check_Var :: Prism' Exp String
check_Var = _Var

check_Lambda :: Prism' Exp (String, Exp)
check_Lambda = _Lambda


declarePrisms [d|
  data Banana = Banana Int String
  |]
-- data Banana = Banana Int String

check_Banana :: Iso' Banana (Int, String)
check_Banana = _Banana

cavendish :: Banana
cavendish = _Banana # (4, "Cavendish")

data family Family a b c

declareLenses [d|
  data instance Family Int (a, b) a = FamilyInt { fm0 :: (b, a), fm1 :: Int }
  |]
-- data instance Family Int (a, b) a = FamilyInt a b
checkFm0 :: Lens (Family Int (a, b) a) (Family Int (a', b') a') (b, a) (b', a')
checkFm0 = fm0

checkFm1 :: Lens' (Family Int (a, b) a) Int
checkFm1 = fm1

declareFieldLabels [d|
  data instance Family Char (a, b) a = FamilyChar { fm0 :: (b, a), fm1 :: Char }
  |]

checkFm0_ :: Lens (Family Char (a, b) a) (Family Char (a', b') a') (b, a) (b', a')
checkFm0_ = #fm0

checkFm1_ :: Lens' (Family Char (a, b) a) Char
checkFm1_ = #fm1

class Class a where
  data Associated a
  method :: a -> Int

declareLenses [d|
  instance Class Int where
    data Associated Int = AssociatedInt { mochi :: Double }
    method = id
  |]

-- instance Class Int where
--   data Associated Int = AssociatedInt Double
--   method = id

checkMochi :: Iso' (Associated Int) Double
checkMochi = mochi

declareFieldLabels [d|
  instance Class Double where
    data Associated Double = AssociatedDouble { coffee :: Double }
    method = floor
  |]

-- instance Class Double where
--   data Associated Double = AssociatedDouble Double
--   method = floor

checkCoffee :: Iso' (Associated Double) Double
checkCoffee = #coffee

declareFields [d|
  data DeclaredFields f a
    = DeclaredField1 { declaredFieldsA0 :: f a    , declaredFieldsB0 :: Int }
    | DeclaredField2 { declaredFieldsC0 :: String , declaredFieldsB0 :: Int }
    deriving (Show)
  |]

checkA0 :: HasA0 t a => AffineTraversal' t a
checkA0 = a0

checkB0 :: HasB0 t a => Lens' t a
checkB0 = b0

checkC0 :: HasC0 t a => AffineTraversal' t a
checkC0 = c0

checkA0' :: AffineTraversal' (DeclaredFields f a) (f a)
checkA0' = a0

checkB0' :: Lens' (DeclaredFields f a) Int
checkB0' = b0

checkC0' :: AffineTraversal' (DeclaredFields f a) String
checkC0' = c0

declareFields [d|
    data Aardvark = Aardvark { aardvarkAlbatross :: Int }
    data Baboon   = Baboon   { baboonAlbatross   :: Int }
  |]

checkAardvark :: Lens' Aardvark Int
checkAardvark = albatross

checkBaboon :: Lens' Baboon Int
checkBaboon = albatross

data Rank2Tests
  = C1 { _r2length :: forall a. [a] -> Int
       , _r2nub    :: forall a. Eq a => [a] -> [a]
       }
  | C2 { _r2length :: forall a. [a] -> Int }

makeLenses ''Rank2Tests
makeFieldLabelsWith lensRules ''Rank2Tests -- doesn't generate anything

checkR2length :: Getter Rank2Tests ([a] -> Int)
checkR2length = r2length

checkR2nub :: Eq a => AffineFold Rank2Tests ([a] -> [a])
checkR2nub = r2nub

data PureNoFields = PureNoFieldsA | PureNoFieldsB { _pureNoFields :: Int }
makeLenses ''PureNoFields
makeFieldLabels ''PureNoFields

data ReviewTest where
  ReviewTest :: (Typeable a, Typeable b) => a -> b -> ReviewTest
makePrisms ''ReviewTest
makePrismLabels ''ReviewTest -- doesn't generate anything

checkReviewTest :: (Typeable a, Typeable b) => Review ReviewTest (a, b)
checkReviewTest = _ReviewTest

-- test FieldNamers

data CheckUnderscoreNoPrefixNamer = CheckUnderscoreNoPrefixNamer
                                    { _fieldUnderscoreNoPrefix :: Int }
makeLensesWith (lensRules & lensField .~ underscoreNoPrefixNamer ) ''CheckUnderscoreNoPrefixNamer
checkUnderscoreNoPrefixNamer :: Iso' CheckUnderscoreNoPrefixNamer Int
checkUnderscoreNoPrefixNamer = fieldUnderscoreNoPrefix

-- how can we test NOT generating a lens for some fields?

data CheckMappingNamer = CheckMappingNamer
                         { fieldMappingNamer :: String }
makeLensesWith (lensRules & lensField .~ (mappingNamer (return . ("hogehoge_" ++)))) ''CheckMappingNamer
checkMappingNamer :: Iso' CheckMappingNamer String
checkMappingNamer = hogehoge_fieldMappingNamer

data CheckLookingupNamer = CheckLookingupNamer
                           { fieldLookingupNamer :: Int }
makeLensesWith (lensRules & lensField .~ (lookingupNamer [("fieldLookingupNamer", "foobarFieldLookingupNamer")])) ''CheckLookingupNamer
checkLookingupNamer :: Iso' CheckLookingupNamer Int
checkLookingupNamer = foobarFieldLookingupNamer

data CheckUnderscoreNamer = CheckUnderscoreNamer
                            { _hogeprefix_fieldCheckUnderscoreNamer :: Int }
makeLensesWith (defaultFieldRules & lensField .~ underscoreNamer) ''CheckUnderscoreNamer
checkUnderscoreNamer :: Lens' CheckUnderscoreNamer Int
checkUnderscoreNamer = fieldCheckUnderscoreNamer

data CheckCamelCaseNamer = CheckCamelCaseNamer
                           { _checkCamelCaseNamerFieldCamelCaseNamer :: Int }
makeLensesWith (defaultFieldRules & lensField .~ camelCaseNamer) ''CheckCamelCaseNamer
checkCamelCaseNamer :: Lens' CheckCamelCaseNamer Int
checkCamelCaseNamer = fieldCamelCaseNamer

data CheckAbbreviatedNamer = CheckAbbreviatedNamer
                             { _hogeprefixFieldAbbreviatedNamer :: Int }
makeLensesWith (defaultFieldRules & lensField .~ abbreviatedNamer ) ''CheckAbbreviatedNamer
checkAbbreviatedNamer :: Lens' CheckAbbreviatedNamer Int
checkAbbreviatedNamer = fieldAbbreviatedNamer

main :: IO ()
main = putStrLn "optics-th-tests: ok"
