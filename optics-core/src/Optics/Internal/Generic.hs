{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
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
  , GSetFieldProd(..)
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
  ) => GSetFieldSum ('PathTree path1 path2) (g1 :+: g2) (h1 :+: h2) b where
  gsetFieldSum (L1 x) = L1 . gsetFieldSum @path1 x
  gsetFieldSum (R1 y) = R1 . gsetFieldSum @path2 y
  {-# INLINE gsetFieldSum #-}

instance
  ( path ~ GSetFieldPath con epath
  , When (IsLeft epath) (HideReps g h)
  , GSetFieldProd path g h b
  ) => GSetFieldSum ('PathLeaf epath) (M1 C ('MetaCons con fix hs) g)
                                      (M1 C ('MetaCons con fix hs) h) b where
  gsetFieldSum (M1 x) = M1 . gsetFieldProd @path x

type family GSetFieldPath (con :: Symbol) (e :: Either Symbol [Path]) :: [Path] where
  GSetFieldPath _   ('Right path) = path
  GSetFieldPath con ('Left name)  = TypeError
    ('Text "Data constructor " ':<>: QuoteSymbol con ':<>:
     'Text " doesn't have a field named " ':<>: QuoteSymbol name)

class GSetFieldProd (path :: [Path]) g h b | path h -> b
                                           , path g b -> h where
  gsetFieldProd :: g x -> b -> h x

-- fast path left
instance {-# OVERLAPPING #-}
  ( GSetFieldProd path g1 h1 b
  ) => GSetFieldProd ('PathLeft : path) (g1 :*: g2) (h1 :*: g2) b where
  gsetFieldProd (x :*: y) = (:*: y) . gsetFieldProd @path x

-- slow path left
instance
  ( GSetFieldProd path g1 h1 b
  , g2 ~ h2
  ) => GSetFieldProd ('PathLeft : path) (g1 :*: g2) (h1 :*: h2) b where
  gsetFieldProd (x :*: y) = (:*: y) . gsetFieldProd @path x

-- fast path right
instance {-# OVERLAPPING #-}
  ( GSetFieldProd path g2 h2 b
  ) => GSetFieldProd ('PathRight : path) (g1 :*: g2) (g1 :*: h2) b where
  gsetFieldProd (x :*: y) = (x :*:) . gsetFieldProd @path y

-- slow path right
instance
  ( GSetFieldProd path g2 h2 b
  , g1 ~ h1
  ) => GSetFieldProd ('PathRight : path) (g1 :*: g2) (h1 :*: h2) b where
  gsetFieldProd (x :*: y) = (x :*:) . gsetFieldProd @path y

instance
  ( r ~ b
  ) => GSetFieldProd '[] (M1 S m (Rec0 a)) (M1 S m (Rec0 b)) r where
  gsetFieldProd _ = M1 . K1

----------------------------------------
-- Affine field

class GAffineFieldImpl (repDefined :: Bool)
                       (name :: Symbol) s t a b | name s -> a
                                             {- These hold morally, but we can't prove it.
                                                , name t -> b
                                                , name s b -> t
                                                , name t a -> s -} where

  gafieldImpl :: AffineTraversal s t a b

instance
  ( Generic s
  , Generic t
  , path ~ GetFieldPaths s name (Rep s)
  , HasField name s a -- require the field to be in scope
  , Unless (AnyHasPath path)
    (TypeError
      ('Text "Type " ':<>: QuoteType s ':<>:
       'Text " doesn't have a field named " ':<>: QuoteSymbol name))
  , GAffineFieldSum path (Rep s) (Rep t) a b
  ) => GAffineFieldImpl 'True name s t a b where
  gafieldImpl = withAffineTraversal
    (atraversalVL (\point f s -> to <$> gafieldSum @path point f (from s)))
    (\match update -> atraversalVL $ \point f s ->
        either point (fmap (update s) . f) (match s))
  {-# INLINE gafieldImpl #-}

----------------------------------------

class GAffineFieldSum (path :: PathTree Symbol) g h a b where
  gafieldSum :: AffineTraversalVL (g x) (h x) a b

instance
  ( GAffineFieldSum path g h a b
  ) => GAffineFieldSum path (M1 D m g) (M1 D m h) a b where
  gafieldSum point f (M1 x) = M1 <$> gafieldSum @path point f x

instance
  ( GAffineFieldSum path1 g1 h1 a b
  , GAffineFieldSum path2 g2 h2 a b
  ) => GAffineFieldSum ('PathTree path1 path2) (g1 :+: g2) (h1 :+: h2) a b where
  gafieldSum point f (L1 x) = L1 <$> gafieldSum @path1 point f x
  gafieldSum point f (R1 y) = R1 <$> gafieldSum @path2 point f y
  {-# INLINE gafieldSum #-}

instance
  ( GAffineFieldMaybe epath g h a b
  ) => GAffineFieldSum ('PathLeaf epath) (M1 C m g) (M1 C m h) a b where
  gafieldSum point f (M1 x) = M1 <$> gafieldMaybe @epath point f x

class GAffineFieldMaybe (epath :: Either Symbol [Path]) g h a b where
  gafieldMaybe :: AffineTraversalVL (g x) (h x) a b

instance
  ( g ~ h
  ) => GAffineFieldMaybe ('Left name) g h a b where
  gafieldMaybe point _ g = point g

instance
  ( GFieldProd prodPath g h a b
  ) => GAffineFieldMaybe ('Right prodPath) g h a b where
  gafieldMaybe _ f g = gfieldProd @prodPath f g

----------------------------------------

class GFieldProd (path :: [Path]) g h a b | path g -> a
                                          , path h -> b
                                          , path g b -> h
                                          , path h a -> g where
  gfieldProd :: LensVL (g x) (h x) a b

-- fast path left
instance {-# OVERLAPPING #-}
  ( GFieldProd path g1 h1 a b
  ) => GFieldProd ('PathLeft : path) (g1 :*: g2) (h1 :*: g2) a b where
  gfieldProd f (x :*: y) = (:*: y) <$> gfieldProd @path f x

-- slow path left
instance
  ( GFieldProd path g1 h1 a b
  , g2 ~ h2
  ) => GFieldProd ('PathLeft : path) (g1 :*: g2) (h1 :*: h2) a b where
  gfieldProd f (x :*: y) = (:*: y) <$> gfieldProd @path f x

-- fast path right
instance {-# OVERLAPPING #-}
  ( GFieldProd path g2 h2 a b
  ) => GFieldProd ('PathRight : path) (g1 :*: g2) (g1 :*: h2) a b where
  gfieldProd f (x :*: y) = (x :*:) <$> gfieldProd @path f y

-- slow path right
instance
  ( GFieldProd path g2 h2 a b
  , g1 ~ h1
  ) => GFieldProd ('PathRight : path) (g1 :*: g2) (h1 :*: h2) a b where
  gfieldProd f (x :*: y) = (x :*:) <$> gfieldProd @path f y

instance
  ( r ~ a
  , s ~ b
  ) => GFieldProd '[] (M1 S m (Rec0 a)) (M1 S m (Rec0 b)) r s where
  gfieldProd f (M1 (K1 x)) = M1 . K1 <$> f x

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
              (TypeError ('Text "There is no 0th position"))
              (GetPositionPaths s n (Rep s))
  , When (n <=? 0) (HideReps (Rep s) (Rep t))
  , GPositionSum path (Rep s) (Rep t) a b
  ) => GPositionImpl 'True n s t a b where
  gpositionImpl = withLens
    (lensVL (\f s -> to <$> gpositionSum @path f (from s)))
    (\get set -> lensVL $ \f s -> set s <$> f (get s))
  {-# INLINE gpositionImpl #-}

----------------------------------------

class GPositionSum (path :: PathTree (Nat, Nat)) g h a b | path g -> a
                                                         , path h -> b
                                                         , path g b -> h
                                                         , path h a -> g where
  gpositionSum :: LensVL (g x) (h x) a b

instance
  ( GPositionSum path g h a b
  ) => GPositionSum path (M1 D m g) (M1 D m h) a b where
  gpositionSum f (M1 x) = M1 <$> gpositionSum @path f x

instance
  ( GPositionSum path1 g1 h1 a b
  , GPositionSum path2 g2 h2 a b
  ) => GPositionSum ('PathTree path1 path2) (g1 :+: g2) (h1 :+: h2) a b where
  gpositionSum f (L1 x) = L1 <$> gpositionSum @path1 f x
  gpositionSum f (R1 y) = R1 <$> gpositionSum @path2 f y
  {-# INLINE gpositionSum #-}

instance
  ( path ~ GPositionPath con epath
  , When (IsLeft epath) (HideReps g h)
  , GFieldProd path g h a b
  ) => GPositionSum ('PathLeaf epath) (M1 C ('MetaCons con fix hs) g)
                                      (M1 C ('MetaCons con fix hs) h) a b where
  gpositionSum f (M1 x) = M1 <$> gfieldProd @path f x

type family GPositionPath con (e :: Either (Nat, Nat) [Path]) :: [Path] where
  GPositionPath _   ('Right path)   = path
  GPositionPath con ('Left '(n, k)) = TypeError
    ('Text "Data constructor " ':<>: QuoteSymbol con ':<>:
     'Text " has " ':<>: ShowFieldNumber k ':<>: 'Text ", " ':<>:
     ToOrdinal n ':<>: 'Text " requested")

type family ShowFieldNumber (k :: Nat) :: ErrorMessage where
  ShowFieldNumber 0 = 'Text "no fields"
  ShowFieldNumber 1 = 'Text "1 field"
  ShowFieldNumber k = 'ShowType k ':<>: 'Text " fields"

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
  , epath ~ GetNamePath name (Rep s) '[]
  , path ~ FromRight
    (TypeError
      ('Text "Type " ':<>: QuoteType s ':<>:
       'Text " doesn't have a constructor named " ':<>: QuoteSymbol name))
    epath
  , When (IsLeft epath) (HideReps (Rep s) (Rep t))
  , GConstructorSum path (Rep s) (Rep t) a b
  ) => GConstructorImpl 'True name s t a b where
  gconstructorImpl = withPrism (generic % gconstructorSum @path) prism
  {-# INLINE gconstructorImpl #-}

----------------------------------------

class GConstructorSum (path :: [Path]) g h a b | path g -> a
                                               , path h -> b
                                               , path g b -> h
                                               , path h a -> g where
  gconstructorSum :: Prism (g x) (h x) a b

instance
  ( GConstructorSum path g h a b
  ) => GConstructorSum path (M1 D m g) (M1 D m h) a b where
  gconstructorSum = _M1 % gconstructorSum @path

-- fast path left
instance {-# OVERLAPPING #-}
  ( GConstructorSum path g1 h1 a b
  ) => GConstructorSum ('PathLeft : path) (g1 :+: g2) (h1 :+: g2) a b where
  gconstructorSum = _L1 % gconstructorSum @path

-- slow path left
instance
  ( GConstructorSum path g1 h1 a b
  , g2 ~ h2
  ) => GConstructorSum ('PathLeft : path) (g1 :+: g2) (h1 :+: h2) a b where
  gconstructorSum = _L1 % gconstructorSum @path

-- fast path right
instance {-# OVERLAPPING #-}
  ( GConstructorSum path g2 h2 a b
  ) => GConstructorSum ('PathRight : path) (g1 :+: g2) (g1 :+: h2) a b where
  gconstructorSum = _R1 % gconstructorSum @path

-- slow path right
instance
  ( GConstructorSum path g2 h2 a b
  , g1 ~ h1
  ) => GConstructorSum ('PathRight : path) (g1 :+: g2) (h1 :+: h2) a b where
  gconstructorSum = _R1 % gconstructorSum @path

instance
  ( GConstructorTuple g h a b
  ) => GConstructorSum '[] (M1 C m g) (M1 C m h) a b where
  gconstructorSum = _M1 % gconstructorTuple

class GConstructorTuple g h a b | g -> a
                                , h -> b
                                , g b -> h
                                , h a -> g where
  gconstructorTuple :: Prism (g x) (h x) a b

-- Fon uncluttering types in below instances a bit.
type F m a = M1 S m (Rec0 a)

instance {-# OVERLAPPABLE #-}
  ( Dysfunctional () () g h a b
  , TypeError
    ('Text "Generic based access supports constructors" ':$$:
     'Text "containing up to 5 fields. Please generate" ':$$:
     'Text "PrismS with Template Haskell if you need more.")
  ) => GConstructorTuple g h a b where
  gconstructorTuple = error "unreachable"

instance
  ( a ~ ()
  , b ~ ()
  ) => GConstructorTuple U1 U1 a b where
  gconstructorTuple = castOptic _U1
  {-# INLINE gconstructorTuple #-}

instance
  ( r ~ a
  , s ~ b
  ) => GConstructorTuple (F m a) (F m b) r s where
  gconstructorTuple = castOptic coerced
  {-# INLINE gconstructorTuple #-}

instance
  ( r ~ (a1, a2)
  , s ~ (b1, b2)
  ) => GConstructorTuple
         (F m1 a1 :*: F m2 a2)
         (F m1 b1 :*: F m2 b2) r s where
  gconstructorTuple = castOptic $ iso
    (\(M1 (K1 a1) :*: M1 (K1 a2)) -> (a1, a2))
    (\(b1, b2) -> M1 (K1 b1) :*: M1 (K1 b2))
  {-# INLINE gconstructorTuple #-}

-- | Only for a derived balanced representation.
instance
  ( r ~ (a1, a2, a3)
  , s ~ (b1, b2, b3)
  ) => GConstructorTuple
         (F m1 a1 :*: F m2 a2 :*: F m3 a3)
         (F m1 b1 :*: F m2 b2 :*: F m3 b3) r s where
  gconstructorTuple = castOptic $ iso
    (\(M1 (K1 a1) :*: M1 (K1 a2) :*: M1 (K1 a3)) -> (a1, a2, a3))
    (\(b1, b2, b3) -> M1 (K1 b1) :*: M1 (K1 b2) :*: M1 (K1 b3))
  {-# INLINE gconstructorTuple #-}

-- | Only for a derived balanced representation.
instance
  ( r ~ (a1, a2, a3, a4)
  , s ~ (b1, b2, b3, b4)
  ) => GConstructorTuple
         ((F m1 a1 :*: F m2 a2) :*: (F m3 a3 :*: F m4 a4))
         ((F m1 b1 :*: F m2 b2) :*: (F m3 b3 :*: F m4 b4)) r s where
  gconstructorTuple = castOptic $ iso
    (\((M1 (K1 a1) :*: M1 (K1 a2)) :*: (M1 (K1 a3) :*: M1 (K1 a4))) -> (a1, a2, a3, a4))
    (\(b1, b2, b3, b4) -> (M1 (K1 b1) :*: M1 (K1 b2)) :*: (M1 (K1 b3) :*: M1 (K1 b4)))
  {-# INLINE gconstructorTuple #-}

-- | Only for a derived balanced representation.
instance
  ( r ~ (a1, a2, a3, a4, a5)
  , s ~ (b1, b2, b3, b4, b5)
  ) => GConstructorTuple
         ((F m1 a1 :*: F m2 a2) :*: (F m3 a3 :*: F m4 a4 :*: F m5 a5))
         ((F m1 b1 :*: F m2 b2) :*: (F m3 b3 :*: F m4 b4 :*: F m5 b5)) r s where
  gconstructorTuple = castOptic $ iso
    (\((M1 (K1 a1) :*: M1 (K1 a2)) :*: (M1 (K1 a3) :*: M1 (K1 a4) :*: M1 (K1 a5))) ->
       (a1, a2, a3, a4, a5))
    (\(b1, b2, b3, b4, b5) ->
       (M1 (K1 b1) :*: M1 (K1 b2)) :*: (M1 (K1 b3) :*: M1 (K1 b4) :*: M1 (K1 b5)))
  {-# INLINE gconstructorTuple #-}

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

instance (Generic s, GPlateImpl (Rep s) a) => GPlateInner 'True s a where
  gplateInner f = fmap to . gplateImpl f . from

instance {-# INCOHERENT #-} GPlateInner repNotDefined s a where
  gplateInner _ = pure

-- $setup
-- >>> import Optics.Core
