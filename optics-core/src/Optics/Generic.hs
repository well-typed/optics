{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module: Optics.Generic
-- Description: Data access via the 'Generic' type class
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

import GHC.Generics (Generic, Rep)
import GHC.TypeLits

import Optics.AffineTraversal
import Optics.Internal.Generic
import Optics.Internal.Magic
import Optics.Internal.Optic
import Optics.Lens
import Optics.Prism
import Optics.Traversal

-- | Hidden type for preventing GHC from solving constraints too early.
data Void0

-- | Focus on a field @name@ of type @a@ within a type @s@ using its 'Generic'
-- instance.
--
-- >>> :{
-- data User a
--   = User { name :: String
--          , age  :: a
--          }
--   | LazyUser { name :: String
--              , age  :: a
--              , lazy :: Bool
--              }
--   deriving (Show, Generic)
-- :}
--
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
-- ...User’ doesn't have a field named ‘salary’
-- ...
--
-- Only total fields are accessible (for partial ones see 'gafield'):
--
-- >>> user ^. gfield @"lazy"
-- ...
-- ...Data constructor ‘User’ doesn't have a field named ‘lazy’
-- ...
--
-- Type changing updates are supported:
--
-- >>> user & gfield @"age" .~ ()
-- User {name = "Tom", age = ()}
--
-- /Note:/ 'gfield' is supported by 'Optics.Label.labelOptic' and can be used
-- with a concise syntax via @OverloadedLabels@.
--
-- >>> user ^. #name
-- "Tom"
--
-- >>> user & #age %~ (+1)
-- User {name = "Tom", age = 33}
--
class GField (name :: Symbol) s t a b | name s -> t a b
                                      , name t -> s a b where
  gfield :: Lens s t a b

instance GFieldContext name s t a b => GField name s t a b where
  gfield = gfieldImpl @name

-- | Hide implementation from haddock.
type GFieldContext name s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GFieldImpl name s t a b
  , Dysfunctional name () s t a b
  )

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
class GAffineField (name :: Symbol) s t a b | name s -> t a b
                                            , name t -> s a b where
  gafield :: AffineTraversal s t a b

instance GAFieldContext name s t a b => GAffineField name s t a b where
  gafield = gafieldImpl @name

-- | Hide implementation from haddock.
type GAFieldContext name s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GAffineFieldImpl name s t a b
  , Dysfunctional name () s t a b
  )

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
class GPosition (n :: Nat) s t a b | n s -> t a b
                                   , n t -> s a b where
  gposition :: Lens s t a b

instance GPositionContext n s t a b => GPosition n s t a b where
  gposition = gpositionImpl @n

-- | Hide implementation from haddock.
type GPositionContext n s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GPositionImpl n s t a b
  , Dysfunctional n () s t a b
  )

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
-- /Note:/ 'gconstructor' is supported by 'Optics.Label.labelOptic' and can be
-- used with a concise syntax via @OverloadedLabels@.
--
-- >>> dog ^? #_Dog
-- Just ("Sparky",2)
--
-- >>> cat & #_Cat % _1 .~ "Merry"
-- Cat {name = "Merry", purrs = True}
--
class GConstructor (name :: Symbol) s t a b | name s -> t a b
                                            , name t -> s a b where
  gconstructor :: Prism s t a b

instance GConstructorContext name s t a b => GConstructor name s t a b where
  gconstructor = gconstructorImpl @name

-- | Hide implementation from haddock.
type GConstructorContext name s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GConstructorImpl name s t a b
  , Dysfunctional name () s t a b
  )
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

instance GPlateContext a s => GPlate a s where
  gplate = traversalVL (gplateInner @'RepDefined)
  {-# INLINE gplate #-}

-- | Hide implementation from haddock.
type GPlateContext a s =
  ( Generic s
  , GPlateImpl (Rep s) a
  )

-- | Hidden instance.
instance GPlate a Void0 where
  gplate = error "unreachable"
-- | Hidden instance.
instance GPlate Void0 a where
  gplate = error "unreachable"

-- $setup
-- >>> :set -XDataKinds -XDeriveGeneric -XStandaloneDeriving -XOverloadedLabels
-- >>> import Optics.Core
