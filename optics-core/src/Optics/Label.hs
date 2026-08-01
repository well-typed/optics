{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module: Optics.Label
-- Description: Overloaded labels as optics.
--
-- Overloaded labels are a solution to Haskell's namespace problem for records.
-- The @-XOverloadedLabels@ extension allows a new expression syntax for labels,
-- a prefix @#@ sign followed by an identifier, e.g. @#foo@.  These expressions
-- can then be given an interpretation that depends on the type at which they
-- are used and the text of the label.
module Optics.Label
  ( -- * How to use labels as optics to make working with Haskell's records more convenient
    --
    -- ** The problem
    -- $problem

    -- ** The solution
    -- $solution

    -- ** The result
    -- $result

    -- * Sample usage
    -- $sampleUsage

    -- * Technical details

    -- ** 'LabelOptic' type class
    LabelOptic(..)
  , LabelOptic'
  , GenericLabelOptics(..)

    -- ** Structure of 'LabelOptic' instances
    -- $instanceStructure

    -- ** Explanation of functional dependencies
    -- $fundepExplanation
  ) where

import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.OverloadedLabels
import GHC.TypeLits

import Optics.Internal.Generic
import Optics.Internal.Magic
import Optics.Internal.Optic

-- $sampleUsage
--
-- #usage#
--
-- An example showing how overloaded labels can be used as optics for fields of
-- types having a 'Generic' instance.
--
-- >>> :set -XDeriveAnyClass
-- >>> :set -XDeriveGeneric
-- >>> :set -XDuplicateRecordFields
-- >>> :set -XOverloadedLabels
-- >>> import GHC.Generics (Generic)
--
-- >>> :{
-- data Pet
--   = Cat  { name :: String, age :: Int, lazy :: Bool }
--   | Fish { name :: String, age :: Int, lazy :: Bool }
--   deriving (Show, Generic)
-- :}
--
-- >>> :{
-- data Human = Human
--   { name :: String
--   , age  :: Integer
--   , pets :: [Pet]
--   } deriving (Show, Generic)
-- :}
--
-- Here is some test data:
--
-- >>> :{
-- peter :: Human
-- peter = Human
--   { name = "Peter"
--   , age = 13
--   , pets =
--       [ Fish
--           { name = "Goldie"
--           , age = 1
--           , lazy = False
--           }
--       , Cat
--           { name = "Loopy"
--           , age = 3
--           , lazy = False
--           }
--       , Cat
--           { name = "Sparky"
--           , age = 2
--           , lazy = True
--           }
--       ]
--   }
-- :}
--
-- Now we can ask for Peter's name:
--
-- >>> peter ^. #name
-- "Peter"
--
-- or for names of his pets:
--
-- >>> peter ^.. #pets % folded % #name
-- ["Goldie","Loopy","Sparky"]
--
-- We can check whether any of his pets is lazy:
--
-- >>> orOf (#pets % folded % #lazy) peter
-- True
--
-- or how things might be be a year from now:
--
-- >>> peter & #age %~ (+1) & #pets % mapped % #age %~ (+1)
-- Human {name = "Peter", age = 14, pets = [Fish {name = "Goldie", age = 2, lazy = False},Cat {name = "Loopy", age = 4, lazy = False},Cat {name = "Sparky", age = 3, lazy = True}]}
--
-- Perhaps Peter is going on vacation and needs to leave his pets at home:
--
-- >>> peter & #pets .~ []
-- Human {name = "Peter", age = 13, pets = []}

-- $problem
--
-- Standard Haskell records are a common source of frustration amongst seasoned
-- Haskell programmers. Their main issues are:
--
-- (1) Inability to define multiple data types sharing field names in the same
--     module.
--
-- (2) Pollution of global namespace as every field accessor is also a top-level
--     function.
--
-- (3) Clunky update syntax, especially when nested fields get involved.
--
-- GHC 9.2 and later provides the following trifecta of language extensions to
-- alleviate them:
--
-- - @<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/duplicate_record_fields.html DuplicateRecordFields>@ - addresses (1)
--
-- - @<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/field_selectors.html NoFieldSelectors>@ and @<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html OverloadedRecordDot>@ - address (2)
--
-- - @<https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_update.html OverloadedRecordUpdate>@ - addresses (3)
--
-- It needs to be noted however that @OverloadedRecordUpdate@ is still not
-- usable out of the box as it requires the user to enable @RebindableSyntax@
-- and provide their own @HasField@ class.
--
-- Labels as optics are an alternative that addresses all three issues (and
-- more) at the cost of a slightly more verbose syntax.

-- $solution
--
-- === Prefixless fields with @DuplicateRecordFields@
--
-- We necessarily want field names to be prefixless, i.e. @field@ to be a field
-- name and @#field@ to be an overloaded label that becomes an optic refering to
-- this field in the appropriate context.  With this approach we get working
-- autocompletion and jump-to-definition in editors supporting @ctags@/@etags@
-- in combination with @<https://hackage.haskell.org/package/ghc-tags ghc-tags>@,
-- both of which (especially the latter) are very important for developer's
-- productivity in real-world code bases.
--
-- Let's look at data types defined with this approach in mind:
--
-- @
-- {-\# LANGUAGE DuplicateRecordFields \#-}
--
-- import Data.Time
--
-- data User = User
--   { id     :: Int
--   , name   :: String
--   , joined :: UTCTime
--   , movies :: [Movie]
--   }
--
-- data Movie = Movie
--   { id          :: Int
--   , name        :: String
--   , releaseDate :: UTCTime
--   }
-- @
--
-- Then appropriate 'LabelOptic' instances can be either written by hand,
-- seamlessly derived via generic representation (see the
-- <Optics-Label.html#usage Sample usage> section for more details)
-- or generated with Template Haskell functions
-- (defined in
-- <https://hackage.haskell.org/package/optics-th/docs/Optics-TH.html Optics.TH>
-- module from <https://hackage.haskell.org/package/optics-th optics-th>
-- package) with
--
-- @
-- makeFieldLabelsNoPrefix ''User
-- makeFieldLabelsNoPrefix ''Movie
-- @
--
-- Generally speaking, both deriving techniques trade blows in terms of compile
-- time and run time resources. Generic optics are a bit slower to compile
-- without optimizations than Template Haskell generated ones. On the other
-- hand, they are much more developer friendly.
--
-- /Note:/ there exists a similar approach that involves prefixing field names
-- (either with the underscore or name of the data type) and generation of
-- lenses as ordinary functions so that @prefixField@ is the ordinary field name
-- and @field@ is the lens referencing it. The drawback of such solution is
-- inability to get working jump-to-definition for field names, which makes
-- navigation in unfamiliar code bases significantly harder, so it's not
-- recommended.
--
-- === Clean namespace with @NoFieldSelectors@
--
-- Prefixless fields (especially ones with common names such as @id@ or @name@)
-- leak into global namespace as accessor functions and can generate a lot of
-- name clashes. The @NoFieldSelectors@ language extension takes care of that:
-- fields of data types defined in a module with this extension enabled can
-- still be used in record construction, pattern matching and record update
-- syntax, but no top-level accessor functions are generated for them.
--
-- An example:
--
-- @
-- {-\# LANGUAGE DataKinds \#-}
-- {-\# LANGUAGE DuplicateRecordFields \#-}
-- {-\# LANGUAGE FlexibleInstances \#-}
-- {-\# LANGUAGE MultiParamTypeClasses \#-}
-- {-\# LANGUAGE NoFieldSelectors \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE TypeFamilies \#-}
-- {-\# LANGUAGE UndecidableInstances \#-}
-- module User (User(..)) where
--
-- import Optics.TH
--
-- data User = User
--   { id   :: Int
--   , name :: String
--   }
--
-- makeFieldLabelsNoPrefix ''User
-- @
--
-- Construction and pattern matching keep using the record syntax:
--
-- @
-- import User
--
-- newUser :: User
-- newUser = User
--   { id   = 1
--   , name = \"Ian\"
--   }
-- @
--
-- whereas reading and modification of fields is done with labels:
--
-- @
-- {-\# LANGUAGE OverloadedLabels \#-}
--
-- import Optics
-- import User
--
-- greetUser :: User -> String
-- greetUser user = "Hello " ++ user ^. #name ++ "!"
--
-- addSurname :: String -> User -> User
-- addSurname surname user = user & #name %~ (++ " " ++ surname)
-- @
--
-- /Note:/ if you plan to completely hide (some of) the fields of a data type,
-- you need to skip defining the corresponding 'LabelOptic' instances for them
-- (in case you want fields to be read only, you can make the optic kind of the
-- coresponding 'LabelOptic' 'A_Getter' instead of 'A_Lens'). It's because
-- Haskell makes it impossible to selectively hide instances, so once a
-- 'LabelOptic' instance is defined, it'll always be possible to use a label
-- that desugars to its usage whenever a module with its definition is
-- (transitively) imported.

-- $result
--
-- When we follow the above conventions for data types in our application, we
-- get:
--
-- (1) Prefixless field names that don't pollute global namespace (courtesy of
--     @NoFieldSelectors@).
--
-- (2) Working tags based jump-to-definition for field names (as @field@ is the
--     ordinary field, whereas @#field@ is the lens referencing it).
--
-- (3) The full power of optics at our disposal, should we ever need it.

-- $instanceStructure #structure#
--
-- You might wonder why instances generated with Template Haskell have the
-- following form:
--
-- @
-- instance (k ~ A_Lens, a ~ [Pet], b ~ [Pet]) => LabelOptic "pets" k Human Human a b where
--   ...
-- @
--
-- instead of
--
-- @
-- instance LabelOptic "pets" A_Lens Human Human [Pet] [Pet] where
--   ...
-- @
--
-- The reason is that using the first form ensures that it is enough for GHC to
-- match on the instance if either @s@ or @t@ is known (as equality constraints
-- are solved after the instance matches), which not only makes type inference
-- better, but also allows it to generate better error messages.
--
-- Therefore, if you write 'LabelOptic' instances by hand, follow the same
-- pattern:
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :{
-- data Pet
--   = Dog {name :: String}
--   | Cat {name :: String}
--   deriving (Show)
-- :}
--
-- >>> :{
-- data Person = Person {pets :: [Pet]}
--   deriving (Show)
-- instance (k ~ A_Lens, a ~ [Pet], b ~ [Pet]) => LabelOptic "pets" k Person Person a b where
--   labelOptic = lensVL $ \f (Person pets) -> Person <$> f pets
-- :}
--
-- >>> let person = Person [Dog "Lucky"]
--
-- Since GHC matches on the instance as soon as it sees @Person@, values whose
-- type it wouldn't be able to infer on its own (such as an empty list, which
-- has the type @[r]@, not @[Pet]@) need no annotations:
--
-- >>> person & #pets .~ []
-- Person {pets = []}
--
-- and using the label at a wrong type or optic kind reports the actual mismatch
-- instead of a missing/overlapping 'LabelOptic' instance:
--
-- >>> person & #pets .~ 'x'
-- ...
-- ...Couldn't match type ‘Char’ with ‘[Pet]’
-- ...  arising from the overloaded label ‘#pets’
-- ...
--
-- >>> let pets = #pets :: Iso' Person [Pet]
-- ...
-- ...Couldn't match type ‘An_Iso’ with ‘A_Lens’
-- ...  arising from the overloaded label ‘#pets’
-- ...

-- $fundepExplanation
--
-- 'LabelOptic' uses the following functional dependencies to guarantee good
-- type inference:
--
-- 1. @name s -> k a@ (the optic for the field @name@ in @s@ is of type @k@ and
-- focuses on @a@)
--
-- 2. @name t -> k b@ (the optic for the field @name@ in @t@ is of type @k@ and
-- focuses on @b@)
--
-- 3. @name s b -> t@ (replacing the field @name@ in @s@ with @b@ yields @t@)
--
-- 4. @name t a -> s@ (replacing the field @name@ in @t@ with @a@ yields @s@)
--
-- Dependencies (1) and (2) ensure that when we compose two optics, the middle
-- type is unambiguous.
--
-- Dependencies (3) and (4) ensure that when we perform a chain of updates, the
-- middle type is unambiguous.

----------------------------------------
-- Definitions

-- | Support for overloaded labels as optics.
--
-- An overloaded label @#foo@ can be used as an optic if there is an instance
-- @'LabelOptic' "foo" k s t a b@.
--
-- Alternatively, if both @s@ and @t@ have a 'Generic' ('GenericLabelOptics' if
-- @explicit-generic-labels@ flag is enabled) instance, a total field of @s@ is
-- accessible by a label @#field@ of kind 'A_Lens', whereas its constructor by a
-- label @#_Constructor@ of kind 'A_Prism'.
class LabelOptic (name :: Symbol) k s t a b | name s -> k a
                                            , name t -> k b
                                            , name s b -> t
                                            , name t a -> s where
  -- | Used to interpret overloaded label syntax.  An overloaded label @#foo@
  -- corresponds to @'labelOptic' \@"foo"@.
  labelOptic :: Optic k NoIx s t a b

-- | Type synonym for a type-preserving optic as overloaded label.
type LabelOptic' name k s a = LabelOptic name k s s a a

data Void0
-- | If for an overloaded label @#label@ there is no instance starting with
-- @LabelOptic "label"@ in scope, using it in the context of optics makes GHC
-- immediately pick the overlappable instance defined below (since no other
-- instance could match). If at this point GHC has no information about @s@ or
-- @t@, it ends up picking incoherent instance of 'GenericLabelOptic' defined
-- below. Prevent that (if only to be able to inspect most polymorphic types of
-- @#foo % #bar@ or @view #foo@ in GHCi) by defining a dummy instance that
-- matches all names, thus postponing instance resolution until @s@ or @t@ is
-- known.
instance
  ( k ~ An_Iso, a ~ Void0, b ~ Void0
  ) => LabelOptic name k Void0 Void0 a b where
  labelOptic = Optic id

-- | If no instance matches, try to use 'Generic' machinery for field access.
--
-- For more information have a look at 'Optics.Generic.gfield' and
-- 'Optics.Generic.gconstructor'.
--
-- @since 0.4
instance {-# OVERLAPPABLE #-}
  ( GenericLabelOpticContext repDefined name k s t a b
  ) => LabelOptic name k s t a b where
  labelOptic = genericOptic @repDefined @name

-- | Hide implementation from haddock.
type GenericLabelOpticContext repDefined name k s t a b =
  ( s `HasShapeOf` t
  , t `HasShapeOf` s
#ifdef EXPLICIT_GENERIC_LABELS
  , repDefined ~ (HasGenericLabelOptics s && HasGenericLabelOptics t)
#else
  , repDefined ~ (Defined (Rep s) && Defined (Rep t))
#endif
  , Unless repDefined (NoLabelOpticError name k s t a b)
  -- If a label starts with "_[A-Z]", assume it's a name of a constructor.
  -- Otherwise, if it starts with "?[a-z]", assume it's a name of a partial
  -- field. Otherwise it's a total field.
  , k ~ If (CmpSymbol "_@" name == LT && CmpSymbol "_[" name == GT)
           A_Prism
           (If (CmpSymbol "?`" name == LT && CmpSymbol "?{" name == GT)
               An_AffineTraversal
               A_Lens)
  , GenericOptic repDefined name k s t a b
  , Dysfunctional name k s t a b
  )

-- | If there is no specific 'LabelOptic' instance, display a custom type error.
type family NoLabelOpticError name k s t a b where
  NoLabelOpticError name k s t a b = TypeError
    (Text "No instance for LabelOptic " :<>: ShowType name
     :<>: Text " " :<>: QuoteType k
     :<>: Text " " :<>: QuoteType s
     :<>: Text " " :<>: QuoteType t
     :<>: Text " " :<>: QuoteType a
     :<>: Text " " :<>: QuoteType b
     :$$: Text "Possible solutions:"
     :$$: Text "- Check and correct spelling of the label"
     :$$: Text "- Define the LabelOptic instance by hand or via Template Haskell"
#ifdef EXPLICIT_GENERIC_LABELS
     :$$: Text "- Derive a GenericLabelOptics instance for " :<>: QuoteType s
#else
     :$$: Text "- Derive a Generic instance for " :<>: QuoteType s
#endif
    )

----------------------------------------

-- | If the @explicit-generic-labels@ Cabal flag is enabled, only types with
-- this instance (which can be trivially derived with @DeriveAnyClass@
-- extension) will be able to use labels as generic optics with a specific type.
--
-- It's an option for application developers to disable implicit fallback to
-- generic optics for more control.
--
-- Libraries using generic labels with their data types should derive this
-- instance for compatibility with the @explicit-generic-labels@ flag.
--
-- /Note:/ the flag @explicit-generic-labels@ is disabled by default. Enabling
-- it is generally unsupported as it might lead to compilation errors of
-- dependencies relying on implicit fallback to generic optics.
--
-- @since 0.4
class Generic a => GenericLabelOptics a where
  type HasGenericLabelOptics a :: Bool
  type HasGenericLabelOptics a = True

----------------------------------------

class GenericOptic (repDefined :: Bool) name k s t a b where
  genericOptic :: Optic k NoIx s t a b

instance
  ( -- We always let GHC enter the GFieldImpl instance because doing so doesn't
    -- generate any additional error messages and we might get type improvements
    -- from the HasField constraint to show in the error message.
    GFieldImpl name s t a b
  ) => GenericOptic repDefined name A_Lens s t a b where
  genericOptic = gfieldImpl @name

-- | This instance can only be used via label syntax with GHC >= 9.6 since it's
-- the first release with unrestricted overloaded labels.
instance
  ( GAffineFieldImpl repDefined name s t a b
  , origName ~ AppendSymbol "?" name
  ) => GenericOptic repDefined origName An_AffineTraversal s t a b where
  genericOptic = gafieldImpl @repDefined @name

instance
  ( GConstructorImpl repDefined name s t a b
  , origName ~ AppendSymbol "_" name
  ) => GenericOptic repDefined origName A_Prism s t a b where
  genericOptic = gconstructorImpl @repDefined @name

----------------------------------------

instance
  (LabelOptic name k s t a b, is ~ NoIx
  ) => IsLabel name (Optic k is s t a b) where
  fromLabel = labelOptic @name

-- $setup
-- >>> import Optics.Core
