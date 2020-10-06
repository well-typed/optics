{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module: Optics.Generic
-- Description: Data access via the Generic type class
--
-- Data access via the 'Generic' type class.
--
module Optics.Generic
  (
  -- * Fields
    GField(..)
  , GAffineField(..)

  -- * Positions
  , GPosition(..)

  -- * Constructors
  , GConstructor(..)

  -- * Types
  , GPlate(..)
  ) where

import Data.Kind
import Data.Type.Bool
import GHC.Generics (Generic, Rep, from, to)
import GHC.TypeLits

import Optics.AffineTraversal
import Optics.Internal.Generic
import Optics.Internal.Optic
import Optics.Lens
import Optics.Prism
import Optics.Traversal

-- | Hidden type for preventing GHC from solving constraints too early.
data Void0

-- | Focus on a field @name@ of type @a@ within a type @s@ using its 'Generic'
-- instance.
--
-- >>> data User a = User { name :: String, age :: a } deriving (Show, Generic)
-- >>> let user = User "Tom" 32 :: User Int
--
-- >>> user ^. gfield @"name"
-- "Tom"
--
-- >>> user ^. gfield @"age"
-- 32
--
-- >>> user ^. gfield @"salary"
-- ...
-- ...Data constructor ‘User’ doesn't have a field named ‘salary’
-- ...
--
-- /Note:/ Type changing updates are also supported:
--
-- >>> user & gfield @"age" .~ ()
-- User {name = "Tom", age = ()}
--
class GField (name :: Symbol) s t a b | name s -> a
                                      , name t -> b
                                      , name s b -> t
                                      , name t a -> s where
  gfield :: Lens s t a b

instance
  ( GField name s t a b
  , Generic s
  , Generic t
  , UnifyHead s t
  , UnifyHead t s
  , path ~ GetFieldPaths s name (Rep s)
  , GFieldSum name path (Rep s) (Rep t) a b
  ) => GField name s t a b where
  gfield = withLens
    (lensVL (\f s -> to <$> gfieldSum @name @path f (from s)))
    (\get set -> lensVL $ \f s -> set s <$> f (get s))
  {-# INLINE gfield #-}

-- | Hidden instance.
instance (a ~ Void0, b ~ Void0) => GField name Void0 Void0 a b where
  gfield = lensVL id

-- | Focus on a possibly partial field @name@ of type @a@ within a type @s@
-- using its 'Generic' instance.
--
-- >>> :{
-- data Fish = Herring { name :: String }
--           | Tuna    { name :: String, sleeping :: Bool }
--   deriving Generic
-- :}
--
-- >>> let herring = Herring { name = "Henry" }
-- >>> let tuna    = Tuna { name = "Tony", sleeping = True }
--
-- >>> herring ^? gafield @"name"
-- Just "Henry"
--
-- >>> herring ^? gafield @"sleeping"
-- Nothing
--
-- >>> tuna ^? gafield @"sleeping"
-- Just True
--
-- /Note:/ trying to access a field that doesn't exist in any data constructor
-- results in an error:
--
-- >>> tuna ^? gafield @"salary"
-- ...
-- ...Type ‘Fish’ doesn't have a field named ‘salary’
-- ...
--
class GAffineField (name :: Symbol) s t a b | name s -> a
                                            , name t -> b
                                            , name s b -> t
                                            , name t a -> s where
  gafield :: AffineTraversal s t a b

instance
  ( GAffineField name s t a b
  , Generic s
  , Generic t
  , UnifyHead s t
  , UnifyHead t s
  , path ~ GetFieldPaths s name (Rep s)
  , If (AnyHasPath path)
       (() :: Constraint)
       (TypeError
        ('Text "Type " ':<>: QuoteType s ':<>:
         'Text " doesn't have a field named " ':<>: QuoteSymbol name))
  , GAffineFieldSum path (Rep s) (Rep t) a b
  ) => GAffineField name s t a b where
  gafield = withAffineTraversal
    (atraversalVL (\point f s -> to <$> gafieldSum @path point f (from s)))
    (\match update -> atraversalVL $ \point f s ->
        either point (fmap (update s) . f) (match s))
  {-# INLINE gafield #-}

-- | Hidden instance.
instance (a ~ Void0, b ~ Void0) => GAffineField name Void0 Void0 a b where
  gafield = atraversalVL (\_ _ -> \case {})

-- | Focus on a field at position @n@ of type @a@ within a type @s@ using its
-- 'Generic' instance.
--
-- >>> ('a', 'b', 'c') ^. gposition @2
-- 'b'
--
-- >>> ('a', 'b') & gposition @1 .~ "hi" & gposition @2 .~ "there"
-- ("hi","there")
--
-- >>> ('a', 'b', 'c') ^. gposition @4
-- ...
-- ...Data constructor ‘(,,)’ has 3 fields, 4th requested
-- ...
--
class GPosition (n :: Nat) s t a b | n s -> a
                                   , n t -> b
                                   , n s b -> t
                                   , n t a -> s where
  gposition :: Lens s t a b

instance
  ( GPosition n s t a b
  , Generic s
  , Generic t
  , UnifyHead s t
  , UnifyHead t s
  , path ~ If (n <=? 0)
              (TypeError ('Text "There is no 0th position"))
              (GetPositionPaths s n (Rep s))
  , GPositionSum n path (Rep s) (Rep t) a b
  ) => GPosition n s t a b where
  gposition = withLens
    (lensVL (\f s -> to <$> gpositionSum @n @path f (from s)))
    (\get set -> lensVL $ \f s -> set s <$> f (get s))
  {-# INLINE gposition #-}

-- | Hidden instance.
instance (a ~ Void0, b ~ Void0) => GPosition name Void0 Void0 a b where
  gposition = lensVL id

-- | Focus on a constructor @name@ of a type @s@ using its 'Generic' instance.
--
-- >>> :{
-- data Animal = Dog { name :: String, age :: Int }
--             | Cat { name :: String, purrs :: Bool }
--   deriving (Show, Generic)
-- :}
--
-- >>> let dog = Dog "Sparky" 2
-- >>> let cat = Cat "Cuddly" True
--
-- >>> dog ^? gconstructor @"Dog"
-- Just ("Sparky",2)
--
-- >>> dog ^? gconstructor @"Cat"
-- Nothing
--
-- >>> cat & gconstructor @"Cat" % _2 %~ not
-- Cat {name = "Cuddly", purrs = False}
--
-- >>> dog & gconstructor @"Cat" % _1 .~ "Merry"
-- Dog {name = "Sparky", age = 2}
--
-- >>> cat ^? gconstructor @"Parrot"
-- ...
-- ...Type ‘Animal’ doesn't have a constructor named ‘Parrot’
-- ...
--
class GConstructor (name :: Symbol) s t a b | name s -> a
                                            , name t -> b
                                            , name s b -> t
                                            , name t a -> s where
  gconstructor :: Prism s t a b

instance
  ( GConstructor name s t a b
  , Generic s
  , Generic t
  , UnifyHead s t
  , UnifyHead t s
  , path ~ FromRight
      (TypeError
        ('Text "Type " ':<>: QuoteType s ':<>:
         'Text " doesn't have a constructor named " ':<>: QuoteSymbol name))
      (GetNamePath name (Rep s) '[])
  , GConstructorSum path (Rep s) (Rep t) a b
  ) => GConstructor name s t a b where
  gconstructor = withPrism (generic % gconstructorSum @path) prism
  {-# INLINE gconstructor #-}

-- | Hidden instance.
instance (a ~ Void0, b ~ Void0) => GConstructor name Void0 Void0 a b where
  gconstructor = prism id (\case {})

-- | Traverse occurrences of a type @a@ within a type @s@ using its 'Generic'
-- instance.
--
-- >>> toListOf (gplate @Char) ('h', ((), 'e', Just 'l'), "lo")
-- "hello"
--
-- If @a@ occurs recursively in its own definition, only outermost occurrences
-- of @a@ within @s@ will be traversed:
--
-- >>> toListOf (gplate @String) ("one","two")
-- ["one","two"]
--
-- /Note:/ types without a 'Generic' instance in scope when 'GPlate' class
-- constraint is resolved will not be entered during the traversal.
--
-- >>> newtype NoG = NoG Char
-- >>> let noG = (NoG 'n', (Just 'i', "c"), 'e')
--
-- >>> toListOf (gplate @Char) noG
-- "ice"
--
-- >>> deriving instance Generic NoG
--
-- >>> toListOf (gplate @Char) noG
-- "nice"
--
class GPlate a s where
  gplate :: Traversal' s a

instance
  ( Generic s
  , GPlateImpl (Rep s) a
  ) => GPlate a s where
  gplate = traversalVL (gplateInner @'RepDefined)
  {-# INLINE gplate #-}

-- | Hidden instance.
instance GPlate a Void0 where
  gplate = error "unreachable"
-- | Hidden instance.
instance GPlate Void0 a where
  gplate = error "unreachable"

-- $setup
-- >>> :set -XDataKinds -XDeriveGeneric -XStandaloneDeriving
-- >>> import Optics.Core
