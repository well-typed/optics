{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module: Optics.Generic
-- Description: Data access via the 'Generic' type class.
--
-- This module provides, for data types having a 'Generic' instance, a way to
-- focus on:
--
-- - their named total fields via 'gfield',
--
-- - their named partial fields via 'gafield',
--
-- - their constructors via 'gconstructor',
--
-- - their fields at a specific position via 'gposition',
--
-- - their fields of a specific type via 'gplate'.
--
-- /Note:/ 'gfield' and 'gconstructor' are supported by
-- 'Optics.Label.labelOptic' and can be used with a consise syntax via
-- @OverloadedLabels@.
--
-- If you're looking for optics for working with a generic representation of a
-- data type, there's "GHC.Generics.Optics".
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

  -- * Comparison with @generic-optics@
  -- $comparison

  -- ** Performance
  -- $performance

  -- ** User experience
  -- $userExperience
  --
  ) where

import Data.Type.Bool
import GHC.Generics (Generic, Rep)
import GHC.TypeLits

import Optics.AffineTraversal
import Optics.Internal.Generic
import Optics.Internal.Magic
import Optics.Internal.Optic
import Optics.Lens
import Optics.Prism
import Optics.Traversal

-- $comparison
--
-- Functions from this module seem to duplicate functionality of the
-- @generic-optics@ library. However, they:
--
-- 1. Integrate with 'Optics.Label.labelOptic' for use via @OverloadedLabels@.
--
-- 2. Are more efficient at both compile and run time than their
--    @generic-optics@ equivalents.
--
-- 3. Provide better user experience (no multiple variants of the same accessor
-- type) and excellent type inference properties.

-- $performance
--
-- Disclaimer: the focus is on data types with one constructor since they occur
-- most often.
--
-- === Compile time
--
-- The test involves compiling a module that defines:
--
-- - A data type with a single constructor and @N@ fields.
--
-- - @N@ top-level lenses, one for each field.
--
-- Testing environment:
--
-- - CPU: Intel Core i7 3770
--
-- - GHC: 8.10.3 (RTS flags: @-A32m@)
--
-- - @generic-optics@: @2.0.0.0@
--
-- Compile times with @-O0@ (in seconds):
--
-- +------------+----------+----------+----------+----------+----------+
-- |Fields      | 10       | 20       |    30    |    40    |    50    |
-- +============+==========+==========+==========+==========+==========+
-- |'gfield'    | __0.43__ | __0.69__ | __1.12__ | __1.64__ | __2.17__ |
-- +------------+----------+----------+----------+----------+----------+
-- |@field@     | 0.54     | 1.14     | 2.12     | 3.49     | 5.04     |
-- +------------+----------+----------+----------+----------+----------+
-- |                                                                   |
-- +------------+----------+----------+----------+----------+----------+
-- |'gposition' | __0.48__ | __0.88__ | __1.51__ | __2.36__ | __3.61__ |
-- +------------+----------+----------+----------+----------+----------+
-- |@position@  | 0.93     | 2.97     | 6.30     | 11.2     | 17.6     |
-- +------------+----------+----------+----------+----------+----------+
--
-- Compile times with @-O1@ (in seconds):
--
-- +------------+----------+----------+----------+----------+----------+
-- |Fields      | 10       | 20       |    30    |    40    |    50    |
-- +============+==========+==========+==========+==========+==========+
-- |'gfield'    | __0.64__ | __1.06__ | __1.72__ | __2.63__ | __3.72__ |
-- +------------+----------+----------+----------+----------+----------+
-- |@field@     | 0.85     | 1.77     | 3.24     | 5.11     | 7.70     |
-- +------------+----------+----------+----------+----------+----------+
-- |                                                                   |
-- +------------+----------+----------+----------+----------+----------+
-- |'gposition' | __0.60__ | __1.18__ | __2.01__ | __3.12__ | __4.47__ |
-- +------------+----------+----------+----------+----------+----------+
-- |@position@  | 11.2     | 20.4     | 47.2     | 87.1     | 136      |
-- +------------+----------+----------+----------+----------+----------+
--
-- Observations:
--
-- - `gfield` compiles on average twice as fast as @field@.
--
-- - `gposition` compiles slower than `gfield`, but still offers reasonable
--   times.
--
-- - Compile times of @position@ are big even for small records and seem to
--   increase exponentially with the number of fields, which makes it pretty
--   much unusable for large data types.
--
-- === Run time
--
-- When GHC optimizes intermediate generic representation of a data type away,
-- these functions will offer the same runtime performance as the ones from
-- @generic-optics@.
--
-- Sadly, for types with one constructor this happens only if the type in
-- question has 10 or less fields (*). For larger ones generic representation is
-- not optimized away and access to a field becomes a few times slower (exact
-- factor depends on the size of a data type).
--
-- However, 'gfield' uses the 'GHC.Records.getField' function for reading a
-- field and generics-based access only for modification (unlike @field@), so
-- reading a field with 'gfield' is always fast.
--
-- (*) GHC 9.2 introduces an optimization flag @-finline-generics@ (enabled by
-- default at @-O1@ and higher) that fixes this.

-- $userExperience
--
-- @generic-optics@ provides three variants of each optic:
--
-- - Two variants for type changing updates.
--
-- - One variant for type preserving updates.
--
-- This is confusing. This module provides a single, type changing optic for
-- each type of data accessor with excellent type inference properties.
--
-- >>> newtype Name a = Name { unwrap :: a } deriving Generic
-- >>> data User a = User { name :: Name a, age :: Int  } deriving Generic
--
-- Inference from @s@ for @#age@:
--
-- >>> let f :: Lens (User a) _t _a _b; f = #age
-- ...
-- ...Found type wildcard ‘_t’ standing for ‘User a’
-- ...
-- ...Found type wildcard ‘_a’ standing for ‘Int’
-- ...
-- ...Found type wildcard ‘_b’ standing for ‘Int’
-- ...
--
-- Inference from @t@ for @#age@:
--
-- >>> let f :: Lens _s (User a) _a _b; f = #age
-- ...
-- ...Found type wildcard ‘_s’ standing for ‘User a’
-- ...
-- ...Found type wildcard ‘_a’ standing for ‘Int’
-- ...
-- ...Found type wildcard ‘_b’ standing for ‘Int’
-- ...
--
-- Inference from @s@ for the composition:
--
-- >>> let f :: Lens (User a) _t _a _b; f = #name % #unwrap
-- ...
-- ...Found type wildcard ‘_t’ standing for ‘User a1’
-- ...
-- ...Found type wildcard ‘_a’ standing for ‘a’
-- ...
-- ...Found type wildcard ‘_b’ standing for ‘a1’
-- ...
--
-- Inference from @t@ for the composition:
--
-- >>> let f :: Lens _s (User a) _a _b; f = #name % #unwrap
-- ...
-- ...Found type wildcard ‘_s’ standing for ‘User a1’
-- ...
-- ...Found type wildcard ‘_a’ standing for ‘a1’
-- ...
-- ...Found type wildcard ‘_b’ standing for ‘a’
-- ...
--
-- Inference from @s@ for @#_User@:
--
-- >>> let f :: Prism (User a) _t _a _b; f = #_User
-- ...
-- ...Found type wildcard ‘_t’ standing for ‘User a1’
-- ...
-- ...Found type wildcard ‘_a’ standing for ‘(Name a, Int)’
-- ...
-- ...Found type wildcard ‘_b’ standing for ‘(Name a1, Int)’
-- ...
--
-- Inference from @t@ for @#_User@:
--
-- >>> let f :: Prism _s (User a) _a _b; f = #_User
-- ...
-- ...Found type wildcard ‘_s’ standing for ‘User a1’
-- ...
-- ...Found type wildcard ‘_a’ standing for ‘(Name a1, Int)’
-- ...
-- ...Found type wildcard ‘_b’ standing for ‘(Name a, Int)’
-- ...

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
-- ...Data constructor ‘User’ doesn't have a field named ‘salary’
-- ...In the...
-- ...
--
-- Only total fields are accessible (for partial ones see 'gafield'):
--
-- >>> user ^. gfield @"lazy"
-- ...
-- ...Data constructor ‘User’ doesn't have a field named ‘lazy’
-- ...In the...
-- ...
--
-- Type changing updates are supported:
--
-- >>> user & gfield @"age" .~ ()
-- User {name = "Tom", age = ()}
--
-- Types without a 'Generic' instance are not supported:
--
-- >>> NoG 'x' ^. gfield @"any"
-- ...
-- ...Type ‘NoG’ doesn't have a Generic instance
-- ...In the...
-- ...
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
-- @since 0.4
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
-- Types without a 'Generic' instance are not supported:
--
-- >>> NoG 'x' ^? gafield @"any"
-- ...
-- ...Type ‘NoG’ doesn't have a Generic instance
-- ...In the...
-- ...
--
-- /Note:/ trying to access a field that doesn't exist in any data constructor
-- results in an error:
--
-- >>> tuna ^? gafield @"salary"
-- ...
-- ...Type ‘Fish’ doesn't have a field named ‘salary’
-- ...In the...
-- ...
--
-- @since 0.4
--
class GAffineField (name :: Symbol) s t a b | name s -> t a b
                                            , name t -> s a b where
  gafield :: AffineTraversal s t a b

instance GAFieldContext repDefined name s t a b => GAffineField name s t a b where
  gafield = gafieldImpl @repDefined @name

-- | Hide implementation from haddock.
type GAFieldContext repDefined name s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , repDefined ~ (Defined (Rep s) && Defined (Rep t))
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GAffineFieldImpl repDefined name s t a b
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
-- ...In the...
-- ...
--
-- >>> () ^. gposition @1
-- ...
-- ...Data constructor ‘()’ has no fields, 1st requested
-- ...In the...
-- ...
--
-- Types without a 'Generic' instance are not supported:
--
-- >>> NoG 'x' ^. gposition @1
-- ...
-- ...Type ‘NoG’ doesn't have a Generic instance
-- ...In the...
-- ...
--
-- /Note:/ Positions start from @1@:
--
-- >>> ('a', 'b') ^. gposition @0
-- ...
-- ...There is no 0th position
-- ...In the...
-- ...
--
-- @since 0.4
--
class GPosition (n :: Nat) s t a b | n s -> t a b
                                   , n t -> s a b where
  gposition :: Lens s t a b

instance GPositionContext repDefined n s t a b => GPosition n s t a b where
  gposition = gpositionImpl @repDefined @n

-- | Hide implementation from haddock.
type GPositionContext repDefined n s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , repDefined ~ (Defined (Rep s) && Defined (Rep t))
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GPositionImpl repDefined n s t a b
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
-- ...In the...
-- ...
--
-- Types without a 'Generic' instance are not supported:
--
-- >>> NoG 'x' ^. gconstructor @"NoG"
-- ...
-- ...Type ‘NoG’ doesn't have a Generic instance
-- ...In the...
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
-- @since 0.4
--
class GConstructor (name :: Symbol) s t a b | name s -> t a b
                                            , name t -> s a b where
  gconstructor :: Prism s t a b

instance GConstructorContext repDefined name s t a b => GConstructor name s t a b where
  gconstructor = gconstructorImpl @repDefined @name

-- | Hide implementation from haddock.
type GConstructorContext repDefined name s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
  , repDefined ~ (Defined (Rep s) && Defined (Rep t))
  , Unless (Defined (Rep s)) (NoGenericError s)
  , Unless (Defined (Rep t)) (NoGenericError t)
  , GConstructorImpl repDefined name s t a b
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
-- @since 0.4
--
class GPlate a s where
  gplate :: Traversal' s a

instance GPlateContext a s => GPlate a s where
  gplate = traversalVL (gplateInner @'True)
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
-- >>> :set -XDataKinds -XDeriveGeneric -XNamedWildCards -XStandaloneDeriving -XOverloadedLabels
-- >>> import Optics.Core
-- >>> newtype NoG = NoG { fromNoG :: Char }
