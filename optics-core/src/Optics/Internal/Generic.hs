{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Generic
  ( generic
  , generic1
  , _V1
  , _U1
  , _Par1
  , _Rec1
  , _K1
  , _M1
  , _L1
  , _R1
  -- * Fields
  , GFieldImpl(..)
  , GSetFieldSum(..)
  , GAffineFieldImpl(..)
  , GAffineFieldSum(..)
  , GFieldProd(..)
  -- * Positions
  , GPositionImpl(..)
  , GPositionSum(..)
  -- * Constructors
  , GConstructorImpl(..)
  , GConstructorSum(..)
  , GConstructorTuple(..)
  -- * Types
  , GPlateImpl(..)
  , GPlateInner(..)
  -- * Re-export
  , module Optics.Internal.Generic.TypeLevel
  ) where

import Data.Bifunctor
import Data.Kind
import Data.Type.Bool
import GHC.Generics
import GHC.Records
import GHC.TypeLits

import Optics.AffineTraversal
import Optics.Internal.Generic.TypeLevel
import Optics.Internal.Magic
import Optics.Internal.Optic
import Optics.Iso
import Optics.Lens
import Optics.Prism
import Optics.Traversal

----------------------------------------
-- GHC.Generics

-- | Convert from the data type to its representation (or back)
--
-- >>> view (generic % re generic) "hello" :: String
-- "hello"
--
generic :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b y)
generic = iso from to

-- | Convert from the data type to its representation (or back)
generic1 :: (Generic1 f, Generic1 g) => Iso (f x) (g y) (Rep1 f x) (Rep1 g y)
generic1 = iso from1 to1

_V1 :: Lens (V1 s) (V1 t) a b
_V1 = lensVL (\_ -> \case {})

_U1 :: Iso (U1 p) (U1 q) () ()
_U1 = iso (const ()) (const U1)

_Par1 :: Iso (Par1 p) (Par1 q) p q
_Par1 = coerced

_Rec1 :: Iso (Rec1 f p) (Rec1 g q) (f p) (g q)
_Rec1 = coerced

_K1 :: Iso (K1 i c p) (K1 j d q) c d
_K1 = coerced

_M1 :: Iso (M1 i c f p) (M1 j d g q) (f p) (g q)
_M1 = coerced

_L1 :: Prism ((a :+: c) t) ((b :+: c) t) (a t) (b t)
_L1 = prism L1 reviewer
  where
    reviewer (L1 v) = Right v
    reviewer (R1 v) = Left (R1 v)

_R1 :: Prism ((c :+: a) t) ((c :+: b) t) (a t) (b t)
_R1 = prism R1 reviewer
  where
    reviewer (R1 v) = Right v
    reviewer (L1 v) = Left (L1 v)

----------------------------------------
-- Field

class GFieldImpl (name :: Symbol) s t a b | name s -> a
                                       {- These hold morally, but we can't prove it.
                                          , name t -> b
                                          , name s b -> t
                                          , name t a -> s -} where
  gfieldImpl :: Lens s t a b

instance
  ( Generic s
  , Generic t
  , path ~ GetFieldPaths s name (Rep s)
  , HasField name s a
  , GSetFieldSum path (Rep s) (Rep t) b
  ) => GFieldImpl name s t a b where
  gfieldImpl = lens (getField @name) (\s -> to . gsetFieldSum @path (from s))
  {-# INLINE gfieldImpl #-}

----------------------------------------

class GSetFieldSum (path :: PathTree Symbol) g h b | path h -> b
                                                   , path g b -> h where
  gsetFieldSum :: g x -> b -> h x

instance
  ( GSetFieldSum path g h b
  ) => GSetFieldSum path (M1 D m g) (M1 D m h) b where
  gsetFieldSum (M1 x) = M1 . gsetFieldSum @path x

instance
  ( GSetFieldSum path1 g1 h1 b
  , GSetFieldSum path2 g2 h2 b
  ) => GSetFieldSum (PathNode path1 path2) (g1 :+: g2) (h1 :+: h2) b where
  gsetFieldSum (L1 x) = L1 . gsetFieldSum @path1 x
  gsetFieldSum (R1 y) = R1 . gsetFieldSum @path2 y
  {-# INLINE gsetFieldSum #-}

instance
  ( path ~ GSetFieldPath con epath
  , GFieldProd path g h a b
  ) => GSetFieldSum (PathLeaf epath) (M1 C (MetaCons con fix hs) g)
                                      (M1 C (MetaCons con fix hs) h) b where
  gsetFieldSum (M1 x) = M1 . gfieldProdSet @path x

type family GSetFieldPath (con :: Symbol) (e :: Either Symbol [Path]) :: [Path] where
  GSetFieldPath _   (Right path) = path
  GSetFieldPath con (Left name)  = TypeError
    (Text "Data constructor " :<>: QuoteSymbol con :<>:
     Text " doesn't have a field named " :<>: QuoteSymbol name)

----------------------------------------
-- Affine field

class GAffineFieldImpl (repDefined :: Bool)
                       (name :: Symbol) s t a b
      {- These hold morally, but we can't prove it.
         | name s -> a
         , name t -> b
         , name s b -> t
         , name t a -> s -} where

  gafieldImpl :: AffineTraversal s t a b

instance
  ( Generic s
  , Generic t
  , path ~ GetFieldPaths s name (Rep s)
  -- Below constraint can't be here anymore: since GHC 9.14.1
  -- -Wincomplete-record-selectors generates warnings about partial fields
  -- whenever an appropriate HasField constraint is solved, not whenever its
  -- evidence (ie. the getField method) is used. For more info see
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/26686.
  --
  -- The consequence of this is that the field no longer needs to be in scope
  -- for 'gafield' to work.
  --
  --, HasField name s a -- require the field to be in scope
  , Unless (AnyHasPath path)
    (TypeError
      (Text "Type " :<>: QuoteType s :<>:
       Text " doesn't have a field named " :<>: QuoteSymbol name))
  , GAffineFieldSum path (Rep s) (Rep t) a b
  ) => GAffineFieldImpl True name s t a b where
  gafieldImpl = atraversal
    (\s -> first to (gafieldMatch @path @(Rep s) @(Rep t) @a @b (from s)))
    (\s -> to . gafieldUpdate @path @(Rep s) @(Rep t) @a @b (from s))
  {-# INLINE gafieldImpl #-}

----------------------------------------

class GAffineFieldSum (path :: PathTree Symbol) g h a b | path g b -> h
                                                        , path h a -> g where
  gafieldMatch :: g x -> Either (h x) a
  gafieldUpdate :: g x -> b -> h x

instance
  ( GAffineFieldSum path g h a b
  ) => GAffineFieldSum path (M1 D m g) (M1 D m h) a b where
  gafieldMatch (M1 x) = first M1 (gafieldMatch @path @g @h @a @b x)
  gafieldUpdate (M1 x) = M1 . gafieldUpdate @path @g @h @a @b x

instance
  ( GAffineFieldSum path1 g1 h1 a b
  , GAffineFieldSum path2 g2 h2 a b
  ) => GAffineFieldSum (PathNode path1 path2) (g1 :+: g2) (h1 :+: h2) a b where
  gafieldMatch (L1 x) = first L1 (gafieldMatch @path1 @g1 @h1 @a @b x)
  gafieldMatch (R1 y) = first R1 (gafieldMatch @path2 @g2 @h2 @a @b y)
  {-# INLINE gafieldMatch #-}
  gafieldUpdate (L1 x) = L1 . gafieldUpdate @path1 @g1 @h1 @a @b x
  gafieldUpdate (R1 y) = R1 . gafieldUpdate @path2 @g2 @h2 @a @b y
  {-# INLINE gafieldUpdate #-}

instance
  ( GAffineFieldMaybe epath g h a b
  ) => GAffineFieldSum (PathLeaf epath) (M1 C m g) (M1 C m h) a b where
  gafieldMatch (M1 x) = first M1 (gafieldMaybeMatch @epath @g @h @a @b x)
  gafieldUpdate (M1 x) = M1 . gafieldMaybeUpdate @epath @g @h @a @b x

class GAffineFieldMaybe (epath :: Either Symbol [Path]) g h a b
  | epath g b -> h
  , epath h a -> g where
  gafieldMaybeMatch :: g x -> Either (h x) a
  gafieldMaybeUpdate :: g x -> b -> h x

-- fast path
instance {-# OVERLAPPING #-} GAffineFieldMaybe (Left name) g g a b where
  gafieldMaybeMatch = Left
  gafieldMaybeUpdate g _ = g

-- slow path
instance
  ( g ~ h
  ) => GAffineFieldMaybe (Left name) g h a b where
  gafieldMaybeMatch = Left
  gafieldMaybeUpdate g _ = g

instance
  ( GFieldProd prodPath g h a b
  ) => GAffineFieldMaybe (Right prodPath) g h a b where
  gafieldMaybeMatch = Right . gfieldProdGet @prodPath @g @h
  gafieldMaybeUpdate = gfieldProdSet @prodPath

----------------------------------------

class GFieldProd (path :: [Path]) g h a b | path g -> a
                                          , path h -> b
                                          , path g b -> h
                                          , path h a -> g where
  gfieldProdGet :: g x -> a
  gfieldProdSet :: g x -> b -> h x

-- fast path left
instance {-# OVERLAPPING #-}
  ( GFieldProd path g1 h1 a b
  ) => GFieldProd (PathLeft : path) (g1 :*: g2) (h1 :*: g2) a b where
  gfieldProdGet (x :*: _) = gfieldProdGet @path @g1 @h1 x
  gfieldProdSet (x :*: y) = (:*: y) . gfieldProdSet @path x

-- slow path left
instance
  ( GFieldProd path g1 h1 a b
  , g2 ~ h2
  ) => GFieldProd (PathLeft : path) (g1 :*: g2) (h1 :*: h2) a b where
  gfieldProdGet (x :*: _) = gfieldProdGet @path @g1 @h1 x
  gfieldProdSet (x :*: y) = (:*: y) . gfieldProdSet @path x

-- fast path right
instance {-# OVERLAPPING #-}
  ( GFieldProd path g2 h2 a b
  ) => GFieldProd (PathRight : path) (g1 :*: g2) (g1 :*: h2) a b where
  gfieldProdGet (_ :*: y) = gfieldProdGet @path @g2 @h2 y
  gfieldProdSet (x :*: y) = (x :*:) . gfieldProdSet @path y

-- slow path right
instance
  ( GFieldProd path g2 h2 a b
  , g1 ~ h1
  ) => GFieldProd (PathRight : path) (g1 :*: g2) (h1 :*: h2) a b where
  gfieldProdGet (_ :*: y) = gfieldProdGet @path @g2 @h2 y
  gfieldProdSet (x :*: y) = (x :*:) . gfieldProdSet @path y

instance
  ( r ~ a
  , s ~ b
  ) => GFieldProd '[] (M1 S m (Rec0 a)) (M1 S m (Rec0 b)) r s where
  gfieldProdGet (M1 (K1 x)) = x
  gfieldProdSet _ = M1 . K1

----------------------------------------
-- Position

class GPositionImpl (repDefined :: Bool)
                    (n :: Nat) s t a b | n s -> a
                                    {- These hold morally, but we can't prove it.
                                       , n t -> b
                                       , n s b -> t
                                       , n t a -> s -} where

  gpositionImpl :: Lens s t a b

instance
  ( Generic s
  , Generic t
  , path ~ If (n <=? 0)
              (TypeError (Text "There is no 0th position"))
              (GetPositionPaths s n (Rep s))
  , GPositionSum path (Rep s) (Rep t) a b
  ) => GPositionImpl True n s t a b where
  gpositionImpl = lens
    (\s -> gpositionGet @path @(Rep s) @(Rep t) (from s))
    (\s -> to . gpositionSet @path (from s))
  {-# INLINE gpositionImpl #-}

----------------------------------------

class GPositionSum (path :: PathTree (Nat, Nat)) g h a b | path g -> a
                                                         , path h -> b
                                                         , path g b -> h
                                                         , path h a -> g where
  gpositionGet :: g x -> a
  gpositionSet :: g x -> b -> h x

instance
  ( GPositionSum path g h a b
  ) => GPositionSum path (M1 D m g) (M1 D m h) a b where
  gpositionGet (M1 x) = gpositionGet @path @g @h x
  gpositionSet (M1 x) = M1 . gpositionSet @path x

instance
  ( GPositionSum path1 g1 h1 a b
  , GPositionSum path2 g2 h2 a b
  ) => GPositionSum (PathNode path1 path2) (g1 :+: g2) (h1 :+: h2) a b where
  gpositionGet (L1 x) = gpositionGet @path1 @g1 @h1 x
  gpositionGet (R1 y) = gpositionGet @path2 @g2 @h2 y
  {-# INLINE gpositionGet #-}
  gpositionSet (L1 x) = L1 . gpositionSet @path1 x
  gpositionSet (R1 y) = R1 . gpositionSet @path2 y
  {-# INLINE gpositionSet #-}

instance
  ( path ~ GPositionPath con epath
  , GFieldProd path g h a b
  ) => GPositionSum (PathLeaf epath) (M1 C (MetaCons con fix hs) g)
                                      (M1 C (MetaCons con fix hs) h) a b where
  gpositionGet (M1 x) = gfieldProdGet @path @g @h x
  gpositionSet (M1 x) = M1 . gfieldProdSet @path x

type family GPositionPath con (e :: Either (Nat, Nat) [Path]) :: [Path] where
  GPositionPath _   (Right path)   = path
  GPositionPath con (Left '(n, k)) = TypeError
    (Text "Data constructor " :<>: QuoteSymbol con :<>:
     Text " has " :<>: ShowFieldNumber k :<>: Text ", " :<>:
     ToOrdinal n :<>: Text " requested")

type family ShowFieldNumber (k :: Nat) :: ErrorMessage where
  ShowFieldNumber 0 = Text "no fields"
  ShowFieldNumber 1 = Text "1 field"
  ShowFieldNumber k = ShowType k :<>: Text " fields"

----------------------------------------
-- Constructor

class GConstructorImpl (repDefined :: Bool)
                       (name :: Symbol) s t a b | name s -> a
                                             {- These hold morally, but we can't prove it.
                                                , name t -> b
                                                , name s b -> t
                                                , name t a -> s -} where

  gconstructorImpl :: Prism s t a b

instance
  ( Generic s
  , Generic t
  , epath ~ GetConstructorPath name (Rep s)
  , path ~ FromRight
    (TypeError
      (Text "Type " :<>: QuoteType s :<>:
       Text " doesn't have a constructor named " :<>: QuoteSymbol name))
    epath
  , GConstructorSum path (Rep s) (Rep t) a b
  ) => GConstructorImpl True name s t a b where
  gconstructorImpl = prism
    (\b -> to (gconstructorBuild @path @(Rep s) b))
    (\s -> first to (gconstructorMatch @path (from s)))
  {-# INLINE gconstructorImpl #-}

----------------------------------------

class GConstructorSum (path :: [Path]) g h a b | path g -> a
                                               , path h -> b
                                               , path g b -> h
                                               , path h a -> g where
  gconstructorMatch :: g x -> Either (h x) a
  gconstructorBuild :: b -> h x

instance
  ( GConstructorSum path g h a b
  ) => GConstructorSum path (M1 D m g) (M1 D m h) a b where
  gconstructorMatch (M1 x) = first M1 (gconstructorMatch @path x)
  gconstructorBuild b = M1 (gconstructorBuild @path @g b)

-- fast path left
instance {-# OVERLAPPING #-}
  ( GConstructorSum path g1 h1 a b
  ) => GConstructorSum (PathLeft : path) (g1 :+: g2) (h1 :+: g2) a b where
  gconstructorMatch (L1 x) = first L1 (gconstructorMatch @path x)
  gconstructorMatch (R1 y) = Left (R1 y)
  gconstructorBuild b = L1 (gconstructorBuild @path @g1 b)

-- slow path left
instance
  ( GConstructorSum path g1 h1 a b
  , g2 ~ h2
  ) => GConstructorSum (PathLeft : path) (g1 :+: g2) (h1 :+: h2) a b where
  gconstructorMatch (L1 x) = first L1 (gconstructorMatch @path x)
  gconstructorMatch (R1 y) = Left (R1 y)
  gconstructorBuild b = L1 (gconstructorBuild @path @g1 b)

-- fast path right
instance {-# OVERLAPPING #-}
  ( GConstructorSum path g2 h2 a b
  ) => GConstructorSum (PathRight : path) (g1 :+: g2) (g1 :+: h2) a b where
  gconstructorMatch (R1 y) = first R1 (gconstructorMatch @path y)
  gconstructorMatch (L1 x) = Left (L1 x)
  gconstructorBuild b = R1 (gconstructorBuild @path @g2 b)

-- slow path right
instance
  ( GConstructorSum path g2 h2 a b
  , g1 ~ h1
  ) => GConstructorSum (PathRight : path) (g1 :+: g2) (h1 :+: h2) a b where
  gconstructorMatch (R1 y) = first R1 (gconstructorMatch @path y)
  gconstructorMatch (L1 x) = Left (L1 x)
  gconstructorBuild b = R1 (gconstructorBuild @path @g2 b)

instance
  ( GConstructorTuple g h a b
  ) => GConstructorSum '[] (M1 C m g) (M1 C m h) a b where
  gconstructorMatch (M1 x) = Right (gconstructorToTuple @g @h x)
  gconstructorBuild b = M1 (gconstructorFromTuple @g @h b)

class GConstructorTuple (g :: Type -> Type) (h :: Type -> Type) a b
  | g -> a
  , h -> b
  , g b -> h
  , h a -> g where
  gconstructorToTuple :: g x -> a
  gconstructorFromTuple :: b -> h x

-- Fon uncluttering types in below instances a bit.
type F m a = M1 S m (Rec0 a)

instance {-# OVERLAPPABLE #-}
  ( Dysfunctional () () g h a b
  , TypeError
    (Text "Generic based access supports constructors" :$$:
     Text "containing up to 5 fields. Please generate" :$$:
     Text "PrismS with Template Haskell if you need more.")
  ) => GConstructorTuple g h a b where
  gconstructorToTuple = error "unreachable"
  gconstructorFromTuple = error "unreachable"

instance
  ( a ~ ()
  , b ~ ()
  ) => GConstructorTuple U1 U1 a b where
  gconstructorToTuple _ = ()
  {-# INLINE gconstructorToTuple #-}
  gconstructorFromTuple _ = U1
  {-# INLINE gconstructorFromTuple #-}

instance
  ( r ~ a
  , s ~ b
  ) => GConstructorTuple (F m a) (F m b) r s where
  gconstructorToTuple (M1 (K1 a)) = a
  {-# INLINE gconstructorToTuple #-}
  gconstructorFromTuple b = M1 (K1 b)
  {-# INLINE gconstructorFromTuple #-}

instance
  ( r ~ (a1, a2)
  , s ~ (b1, b2)
  ) => GConstructorTuple
         (F m1 a1 :*: F m2 a2)
         (F m1 b1 :*: F m2 b2) r s where
  gconstructorToTuple (M1 (K1 a1) :*: M1 (K1 a2)) = (a1, a2)
  {-# INLINE gconstructorToTuple #-}
  gconstructorFromTuple (b1, b2) = M1 (K1 b1) :*: M1 (K1 b2)
  {-# INLINE gconstructorFromTuple #-}

-- | Only for a derived balanced representation.
instance
  ( r ~ (a1, a2, a3)
  , s ~ (b1, b2, b3)
  ) => GConstructorTuple
         (F m1 a1 :*: F m2 a2 :*: F m3 a3)
         (F m1 b1 :*: F m2 b2 :*: F m3 b3) r s where
  gconstructorToTuple (M1 (K1 a1) :*: M1 (K1 a2) :*: M1 (K1 a3)) =
    (a1, a2, a3)
  {-# INLINE gconstructorToTuple #-}
  gconstructorFromTuple (b1, b2, b3) =
    M1 (K1 b1) :*: M1 (K1 b2) :*: M1 (K1 b3)
  {-# INLINE gconstructorFromTuple #-}

-- | Only for a derived balanced representation.
instance
  ( r ~ (a1, a2, a3, a4)
  , s ~ (b1, b2, b3, b4)
  ) => GConstructorTuple
         ((F m1 a1 :*: F m2 a2) :*: (F m3 a3 :*: F m4 a4))
         ((F m1 b1 :*: F m2 b2) :*: (F m3 b3 :*: F m4 b4)) r s where
  gconstructorToTuple
    ((M1 (K1 a1) :*: M1 (K1 a2)) :*: (M1 (K1 a3) :*: M1 (K1 a4))) =
    (a1, a2, a3, a4)
  {-# INLINE gconstructorToTuple #-}
  gconstructorFromTuple (b1, b2, b3, b4) =
    (M1 (K1 b1) :*: M1 (K1 b2)) :*: (M1 (K1 b3) :*: M1 (K1 b4))
  {-# INLINE gconstructorFromTuple #-}

-- | Only for a derived balanced representation.
instance
  ( r ~ (a1, a2, a3, a4, a5)
  , s ~ (b1, b2, b3, b4, b5)
  ) => GConstructorTuple
         ((F m1 a1 :*: F m2 a2) :*: (F m3 a3 :*: F m4 a4 :*: F m5 a5))
         ((F m1 b1 :*: F m2 b2) :*: (F m3 b3 :*: F m4 b4 :*: F m5 b5)) r s where
  gconstructorToTuple
    ((M1 (K1 a1) :*: M1 (K1 a2)) :*: (M1 (K1 a3) :*: M1 (K1 a4) :*: M1 (K1 a5))) =
    (a1, a2, a3, a4, a5)
  {-# INLINE gconstructorToTuple #-}
  gconstructorFromTuple (b1, b2, b3, b4, b5) =
    (M1 (K1 b1) :*: M1 (K1 b2)) :*: (M1 (K1 b3) :*: M1 (K1 b4) :*: M1 (K1 b5))
  {-# INLINE gconstructorFromTuple #-}

----------------------------------------
-- Types

class GPlateImpl g a where
  gplateImpl :: TraversalVL' (g x) a

instance GPlateImpl f a => GPlateImpl (M1 i c f) a where
  gplateImpl f (M1 x) = M1 <$> gplateImpl f x

instance (GPlateImpl f a, GPlateImpl g a) => GPlateImpl (f :+: g) a where
  gplateImpl f (L1 x) = L1 <$> gplateImpl f x
  gplateImpl f (R1 x) = R1 <$> gplateImpl f x

instance (GPlateImpl f a, GPlateImpl g a) => GPlateImpl (f :*: g) a where
  gplateImpl f (x :*: y) = (:*:) <$> gplateImpl f x <*> gplateImpl f y
  {-# INLINE gplateImpl #-}

-- | Matching type.
instance {-# OVERLAPPING #-} GPlateImpl (K1 i a) a where
  gplateImpl f (K1 a) = K1 <$> f a

-- | Recurse into the inner type if it has a 'Generic' instance.
instance GPlateInner (Defined (Rep b)) b a => GPlateImpl (K1 i b) a where
  gplateImpl f (K1 b) = K1 <$> gplateInner @(Defined (Rep b)) f b

instance GPlateImpl U1 a where
  gplateImpl _ = pure

instance GPlateImpl V1 a where
  gplateImpl _ = \case {}

instance GPlateImpl (URec b) a where
  gplateImpl _ = pure

class GPlateInner (repDefined :: Bool) s a where
  gplateInner :: TraversalVL' s a

instance (Generic s, GPlateImpl (Rep s) a) => GPlateInner True s a where
  gplateInner f = fmap to . gplateImpl f . from

instance {-# INCOHERENT #-} GPlateInner repNotDefined s a where
  gplateInner _ = pure

-- $setup
-- >>> import Optics.Core
