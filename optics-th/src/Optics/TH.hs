module Optics.TH
  (
  -- * Generation of field optics
  -- ** Labels
    makeFieldLabels
  , makeFieldLabelsFor
  , makeFieldLabelsWith
  , declareFieldLabels
  , declareFieldLabelsFor
  , declareFieldLabelsWith
  , fieldLabelsRules
  , fieldLabelsRulesFor
  -- ** Functions
  , makeLenses
  , makeLensesFor
  , makeLensesWith
  , declareLenses
  , declareLensesFor
  , declareLensesWith
  , lensRules
  , lensRulesFor
  -- ** Single class per data type
  -- $deprecatedClassy
  , makeClassy
  , makeClassy_
  , makeClassyFor
  , declareClassy
  , declareClassyFor
  , classyRules
  , classyRules_
  , classyRulesFor
  -- ** Multiple classes per data type
  -- $deprecatedFields
  , makeFields
  , makeFieldsNoPrefix
  , declareFields
  , defaultFieldRules
  -- * Generation of constructor optics
  -- ** Labels
  , makePrismLabels
  -- ** Functions
  , makePrisms
  , declarePrisms
  -- ** Single class per data type
  , makeClassyPrisms
  -- * Generation rules for field optics
  , LensRules
  , simpleLenses
  , generateSignatures
  , generateUpdateableOptics
  , generateLazyPatterns
  , createClass
  , lensField
  , lensClass
  -- * Common rules
  , noPrefixFieldLabels
  , abbreviatedFieldLabels
  , underscoreFields
  , camelCaseFields
  , classUnderscoreNoPrefixFields
  , abbreviatedFields
  -- * Field namers
  , FieldNamer
  , ClassyNamer
  , DefName(..)
  , noPrefixNamer
  , underscoreNoPrefixNamer
  , lookingupNamer
  , mappingNamer
  , underscoreNamer
  , camelCaseNamer
  , classUnderscoreNoPrefixNamer
  , abbreviatedNamer
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Char (toLower, toUpper, isUpper)
import Data.List as List
import Data.Maybe (maybeToList)
import Data.Monoid
import Data.Set (Set)
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH
import qualified Data.Set as Set

import Optics.Core hiding (cons)
import Optics.TH.Internal.Product
import Optics.TH.Internal.Sum

----------------------------------------
-- Labels

-- | Build field optics as instances of 'LabelOptic' class for use as overloaded
-- labels.
--
-- /e.g./
--
-- @
-- data Animal
--   = Cat { animalAge  :: Int
--         , animalName :: String
--         }
--   | Dog { animalAge    :: Int
--         , animalAbsurd :: forall a b. a -> b
--         }
-- makeFieldLabels ''Animal
-- @
--
-- will create
--
-- @
-- instance
--   (k ~ A_Lens, a ~ Int, b ~ Int
--   ) => LabelOptic "age" k Animal Animal a b where
--   labelOptic = lensVL $ \\f s -> case s of
--     Cat x1 x2 -> fmap (\\y -> Cat y x2) (f x1)
--     Dog x1 x2 -> fmap (\\y -> Dog y x2) (f x1)
--
-- instance
--   (k ~ An_AffineTraversal, a ~ String, b ~ String
--   ) => LabelOptic "name" k Animal Animal a b where
--   labelOptic = atraversalVL $ \\point f s -> case s of
--     Cat x1 x2 -> fmap (\\y -> Cat x1 y) (f x2)
--     Dog x1 x2 -> point (Dog x1 x2)
-- @
--
-- which can be used as @#age@ and @#name@ with language extension
-- OverloadedLabels.
--
-- /Note:/ if you wonder about the form of instances or why there is no label for
-- @animalAbsurd@, check documentation for 'LabelOptic'.
--
-- @
-- 'makeFieldOptics' = 'makeFieldLabelsWith' 'fieldLabelsRules'
-- @
makeFieldLabels :: Name -> DecsQ
makeFieldLabels = makeFieldLabelsWith fieldLabelsRules

-- | Derive field optics as labels, specifying explicit pairings of @(fieldName,
-- labelName)@.
--
-- If you map multiple fields to the same label and it is present in the same
-- constructor, 'Traversal' (or 'Fold' for a read only version) will be
-- generated.
--
-- /e.g./
--
-- @
-- 'makeFieldLabelsFor' [(\"_foo\", \"fooLens\"), (\"baz\", \"lbaz\")] ''Foo
-- 'makeFieldLabelsFor' [(\"_barX\", \"bar\"), (\"_barY\", \"bar\")] ''Bar
-- @
makeFieldLabelsFor :: [(String, String)] -> Name -> DecsQ
makeFieldLabelsFor fields = makeFieldLabelsWith (fieldLabelsRulesFor fields)

-- | Make field optics as labels for all records in the given declaration
-- quote. All record syntax in the input will be stripped off.
--
-- /e.g./
--
-- @
-- declareLenses [d|
--   data Dog = Dog { name :: String, age :: Int }
--     deriving Show
--   |]
-- @
--
-- will create
--
-- @
-- data Dog = Dog String Int
--   deriving Show
-- instance (k ~ A_Lens, ...) => LabelOptic "name" k Dog Dog ...
-- instance (k ~ A_Lens, ...) => LabelOptic "age" k Dog Dog ...
-- @
declareFieldLabels :: DecsQ -> DecsQ
declareFieldLabels
  = declareFieldLabelsWith
  $ fieldLabelsRules
  & lensField .~ noPrefixNamer

-- | Similar to 'makeFieldLabelsFor', but takes a declaration quote.
declareFieldLabelsFor :: [(String, String)] -> DecsQ -> DecsQ
declareFieldLabelsFor fields
  = declareFieldLabelsWith
  $ fieldLabelsRulesFor fields
  & lensField .~ noPrefixNamer

-- Similar to 'makeFieldLabelsWith', but takes a declaration quote.
declareFieldLabelsWith :: LensRules -> DecsQ -> DecsQ
declareFieldLabelsWith rules = declareWith $ \dec -> do
  emit =<< liftDeclare (makeFieldLabelsForDec rules dec)
  return $ stripFields dec

-- | Rules for generation of 'LabelOptic' intances for use with
-- OverloadedLabels. Same as 'lensRules', but uses 'camelCaseNamer'.
--
-- /Note:/ if you don't want to prefix field names with the full name of the
-- data type, you can use 'abbreviatedNamer' instead.
fieldLabelsRules :: LensRules
fieldLabelsRules = LensRules
  { _simpleLenses    = False
  , _generateSigs    = True
  , _generateClasses = False
  , _allowIsos       = True
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = const Nothing
  , _fieldToDef      = camelCaseNamer
  }

-- | Construct a 'LensRules' value for generating 'LabelOptic' instances using
-- the given map from field names to definition names.
fieldLabelsRulesFor
  :: [(String, String)] {- ^ [(Field name, Label name)] -}
  -> LensRules
fieldLabelsRulesFor fields = fieldLabelsRules & lensField .~ lookingupNamer fields

----------------------------------------
-- Lenses

-- | Build field optics as top level functions with a sensible default
-- configuration.
--
-- /e.g./
--
-- @
-- data Animal
--   = Cat { _age  :: 'Int'
--         , _name :: 'String'
--         }
--   | Dog { _age    :: 'Int'
--         , _absurd :: forall a b. a -> b
--         }
-- 'makeLenses' ''Animal
-- @
--
-- will create
--
-- @
-- absurd :: forall a b. AffineFold Animal (a -> b)
-- absurd = afolding $ \\s -> case s of
--   Cat _ _ -> Nothing
--   Dog _ x -> Just x
--
-- age :: Lens' Animal Int
-- age = lensVL $ \\f s -> case s of
--   Cat x1 x2 -> fmap (\\y -> Cat y x2) (f x1)
--   Dog x1 x2 -> fmap (\\y -> Dog y x2) (f x1)
--
-- name :: AffineTraversal' Animal String
-- name = atraversalVL $ \\point f s -> case s of
--   Cat x1 x2 -> fmap (\\y -> Cat x1 y) (f x2)
--   Dog x1 x2 -> point (Dog x1 x2)
-- @
--
-- @
-- 'makeLenses' = 'makeLensesWith' 'lensRules'
-- @
makeLenses :: Name -> DecsQ
makeLenses = makeFieldOptics lensRules

-- | Derive field optics, specifying explicit pairings of @(fieldName,
-- opticName)@.
--
-- If you map multiple fields to the same optic and it is present in the same
-- constructor, 'Traversal' (or 'Fold' for a read only version) will be
-- generated.
--
-- /e.g./
--
-- @
-- 'makeLensesFor' [(\"_foo\", \"fooLens\"), (\"baz\", \"lbaz\")] ''Foo
-- 'makeLensesFor' [(\"_barX\", \"bar\"), (\"_barY\", \"bar\")] ''Bar
-- @
makeLensesFor :: [(String, String)] -> Name -> DecsQ
makeLensesFor fields = makeFieldOptics (lensRulesFor fields)

-- | Build field optics with a custom configuration.
makeLensesWith :: LensRules -> Name -> DecsQ
makeLensesWith = makeFieldOptics

-- | Make field optics for all records in the given declaration quote. All
-- record syntax in the input will be stripped off.
--
-- /e.g./
--
-- @
-- declareLenses [d|
--   data Foo = Foo { fooX, fooY :: 'Int' }
--     deriving 'Show'
--   |]
-- @
--
-- will create
--
-- @
-- data Foo = Foo 'Int' 'Int' deriving 'Show'
-- fooX, fooY :: 'Lens'' Foo Int
-- @
declareLenses :: DecsQ -> DecsQ
declareLenses
  = declareLensesWith
  $ lensRules
  & lensField .~ noPrefixNamer

-- | Similar to 'makeLensesFor', but takes a declaration quote.
declareLensesFor :: [(String, String)] -> DecsQ -> DecsQ
declareLensesFor fields
  = declareLensesWith
  $ lensRulesFor fields
  & lensField .~ noPrefixNamer

-- | 'declareLenses' with custom 'LensRules'.
declareLensesWith :: LensRules -> DecsQ -> DecsQ
declareLensesWith rules = declareWith $ \dec -> do
  emit =<< lift (makeFieldOpticsForDec' rules dec)
  return $ stripFields dec

-- | Rules for making read-write field optics as top-level functions. It uses
-- 'underscoreNoPrefixNamer'.
lensRules :: LensRules
lensRules = LensRules
  { _simpleLenses    = False
  , _generateSigs    = True
  , _generateClasses = False
  , _allowIsos       = True
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = const Nothing
  , _fieldToDef      = underscoreNoPrefixNamer
  }

-- | Construct a 'LensRules' value for generating top-level functions using the
-- given map from field names to definition names.
lensRulesFor
  :: [(String, String)] {- ^ [(Field name, Optic name)] -}
  -> LensRules
lensRulesFor fields = lensRules & lensField .~ lookingupNamer fields

----------------------------------------
-- Classy

-- $deprecatedClassy
--
-- This method of optics generation should only be used when migrating an
-- existing codebase from the @lens@ library to @optics@ as it:
--
-- - Doesn't support prefixless fields.
--
-- - Doesn't support type changing updates.

-- | Make lenses and traversals for a type, and create a class when the type has
-- no arguments.
--
-- /e.g./
--
-- @
-- data Foo = Foo { _fooX, _fooY :: 'Int' }
-- 'makeClassy' ''Foo
-- @
--
-- will create
--
-- @
-- class HasFoo c where
--   foo  :: Lens' c Foo
--   fooX :: Lens' c Int
--   fooY :: Lens' c Int
--   fooX = foo % fooX
--   fooY = foo % fooY
--
-- instance HasFoo Foo where
--   foo  = lensVL id
--   fooX = lensVL $ \\f s -> case s of
--     Foo x1 x2 -> fmap (\\y -> Foo y x2) (f x1)
--   fooY = lensVL $ \\f s -> case s of
--     Foo x1 x2 -> fmap (\\y -> Foo x1 y) (f x2)
-- @
--
-- @
-- 'makeClassy' = 'makeLensesWith' 'classyRules'
-- @
makeClassy :: Name -> DecsQ
makeClassy = makeFieldOptics classyRules

-- | Make lenses and traversals for a type, and create a class when the type has
-- no arguments. Works the same as 'makeClassy' except that (a) it expects that
-- record field names do not begin with an underscore, (b) all record fields are
-- made into lenses, and (c) the resulting lens is prefixed with an underscore.
makeClassy_ :: Name -> DecsQ
makeClassy_ = makeFieldOptics classyRules_

-- | Derive lenses and traversals, using a named wrapper class, and
-- specifying explicit pairings of @(fieldName, traversalName)@.
--
-- Example usage:
--
-- @
-- 'makeClassyFor' \"HasFoo\" \"foo\" [(\"_foo\", \"fooLens\"), (\"bar\", \"lbar\")] ''Foo
-- @
makeClassyFor :: String -> String -> [(String, String)] -> Name -> DecsQ
makeClassyFor clsName funName fields = makeFieldOptics $
  classyRulesFor (const (Just (clsName, funName))) fields

-- | For each record in the declaration quote, make lenses and traversals for
-- it, and create a class when the type has no arguments. All record syntax
-- in the input will be stripped off.
--
-- /e.g./
--
-- @
-- declareClassy [d|
--   data Foo = Foo { fooX, fooY :: 'Int' }
--     deriving 'Show'
--   |]
-- @
--
-- will create
--
-- @
-- data Foo = Foo 'Int' 'Int' deriving 'Show'
-- class HasFoo t where
--   foo :: 'Lens'' t Foo
-- instance HasFoo Foo where foo = 'id'
-- fooX, fooY :: HasFoo t => 'Lens'' t 'Int'
-- @
declareClassy :: DecsQ -> DecsQ
declareClassy
  = declareLensesWith
  $ classyRules
  & lensField .~ noPrefixNamer

-- | Similar to 'makeClassyFor', but takes a declaration quote.
declareClassyFor ::
  [(String, (String, String))] -> [(String, String)] -> DecsQ -> DecsQ
declareClassyFor classes fields
  = declareLensesWith
  $ classyRulesFor (`Prelude.lookup`classes) fields
  & lensField .~ noPrefixNamer

-- | Rules for making lenses and traversals that precompose another 'Lens'.
classyRules :: LensRules
classyRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True
  , _allowIsos       = False -- generating Isos would hinder "subtyping"
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = \n ->
        case nameBase n of
          x:xs -> Just (mkName ("Has" ++ x:xs), mkName (toLower x:xs))
          []   -> Nothing
  , _fieldToDef      = underscoreNoPrefixNamer
  }

-- | A 'LensRules' used by 'makeClassy_'.
classyRules_ :: LensRules
classyRules_
  = classyRules & lensField .~ \_ _ n -> [TopName (mkName ('_':nameBase n))]

-- | Rules for making lenses and traversals that precompose another 'Lens' using
-- a custom function for naming the class, main class method, and a mapping from
-- field names to definition names.
classyRulesFor
  :: (String -> Maybe (String, String)) {- ^ Type Name -> Maybe (Class Name, Method Name) -} ->
  [(String, String)] {- ^ [(Field Name, Method Name)] -} ->
  LensRules
classyRulesFor classFun fields = classyRules
  & lensClass .~ (over (mapped % each) mkName . classFun . nameBase)
  & lensField .~ lookingupNamer fields

----------------------------------------
-- Fields

-- $deprecatedFields
--
-- This method of optics generation should only be used when migrating an
-- existing codebase from the @lens@ library to @optics@ as it:
--
-- - Doesn't support type changing updates.
--
-- - Introduces tight coupling between types in your application as either all
--   types need to be put in a single module (for @HasX@ class generation to
--   work properly) or there needs to be a single, written by hand module with
--   all the @HasX@ classes the application will use. Both approaches don't
--   scale.
--
-- - Can't be leveraged by libraries because of the above problem lifted to the
--   library level: there would have to exist a library with all possible @HasX@
--   classes written by hand that is imported by all the other
--   libraries. Otherwise for a given @field@ independent libraries would
--   provide multiple @HasField@ classes incompatible with each other.

-- | Generate overloaded field accessors.
--
-- /e.g/
--
-- @
-- data Foo a = Foo { _fooX :: 'Int', _fooY :: a }
-- newtype Bar = Bar { _barX :: 'Char' }
-- makeFields ''Foo
-- makeFields ''Bar
-- @
--
-- will create
--
-- @
-- class HasX s a | s -> a where
--   x :: Lens' s a
--
-- instance HasX (Foo a) Int where
--   x = lensVL $ \\f s -> case s of
--     Foo x1 x2 -> fmap (\\y -> Foo y x2) (f x1)
--
-- class HasY s a | s -> a where
--   y :: Lens' s a
--
-- instance HasY (Foo a) a where
--   y = lensVL $ \\f s -> case s of
--     Foo x1 x2 -> fmap (\\y -> Foo x1 y) (f x2)
--
-- instance HasX Bar Char where
--   x = lensVL $ \\f s -> case s of
--     Bar x1 -> fmap (\\y -> Bar y) (f x1)
-- @
--
-- For details, see 'camelCaseFields'.
--
-- @
-- makeFields = 'makeLensesWith' 'defaultFieldRules'
-- @
makeFields :: Name -> DecsQ
makeFields = makeFieldOptics camelCaseFields

-- | Generate overloaded field accessors based on field names which
-- are only prefixed with an underscore (e.g. '_name'), not
-- additionally with the type name (e.g. '_fooName').
--
-- This might be the desired behaviour in case the
-- @DuplicateRecordFields@ language extension is used in order to get
-- rid of the necessity to prefix each field name with the type name.
--
-- As an example:
--
-- @
-- data Foo a  = Foo { _x :: 'Int', _y :: a }
-- newtype Bar = Bar { _x :: 'Char' }
-- makeFieldsNoPrefix ''Foo
-- makeFieldsNoPrefix ''Bar
-- @
--
-- will create classes
--
-- @
-- class HasX s a | s -> a where
--   x :: Lens' s a
-- class HasY s a | s -> a where
--   y :: Lens' s a
-- @
--
-- together with instances
--
-- @
-- instance HasX (Foo a) Int
-- instance HasY (Foo a) a where
-- instance HasX Bar Char where
-- @
--
-- For details, see 'classUnderscoreNoPrefixFields'.
--
-- @
-- makeFieldsNoPrefix = 'makeLensesWith' 'classUnderscoreNoPrefixFields'
-- @
makeFieldsNoPrefix :: Name -> DecsQ
makeFieldsNoPrefix = makeFieldOptics classUnderscoreNoPrefixFields

-- | @ declareFields = 'declareLensesWith' 'defaultFieldRules' @
declareFields :: DecsQ -> DecsQ
declareFields = declareLensesWith defaultFieldRules

defaultFieldRules :: LensRules
defaultFieldRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True  -- classes will still be skipped if they already exist
  , _allowIsos       = False -- generating Isos would hinder field class reuse
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = const Nothing
  , _fieldToDef      = camelCaseNamer
  }

----------------------------------------
-- Prisms

-- | Generate a 'Control.Lens.Type.Prism' for each constructor of each data type.
--
-- /e.g./
--
-- @
-- declarePrisms [d|
--   data Exp = Lit Int | Var String | Lambda{ bound::String, body::Exp }
--   |]
-- @
--
-- will create
--
-- @
-- data Exp = Lit Int | Var String | Lambda { bound::String, body::Exp }
-- _Lit :: 'Prism'' Exp Int
-- _Var :: 'Prism'' Exp String
-- _Lambda :: 'Prism'' Exp (String, Exp)
-- @
declarePrisms :: DecsQ -> DecsQ
declarePrisms = declareWith $ \dec -> do
  emit =<< liftDeclare (makeDecPrisms True dec)
  return dec

----------------------------------------
-- Customization of rules

-- | Generate "simple" optics even when type-changing optics are possible.
-- (e.g. 'Lens'' instead of 'Lens')
simpleLenses :: Lens' LensRules Bool
simpleLenses = lensVL $ \f r ->
  fmap (\x -> r { _simpleLenses = x}) (f (_simpleLenses r))

-- | Indicate whether or not to supply the signatures for the generated lenses.
--
-- Disabling this can be useful if you want to provide a more restricted type
-- signature or if you want to supply hand-written haddocks.
generateSignatures :: Lens' LensRules Bool
generateSignatures = lensVL $ \f r ->
  fmap (\x -> r { _generateSigs = x}) (f (_generateSigs r))

-- | Generate "updateable" optics when 'True'. When 'False', (affine) folds will
-- be generated instead of (affine) traversals and getters will be generated
-- instead of lenses. This mode is intended to be used for types with invariants
-- which must be maintained by "smart" constructors.
generateUpdateableOptics :: Lens' LensRules Bool
generateUpdateableOptics = lensVL $ \f r ->
  fmap (\x -> r { _allowUpdates = x}) (f (_allowUpdates r))

-- | Generate optics using lazy pattern matches. This can
-- allow fields of an undefined value to be initialized with lenses:
--
-- @
-- data Foo = Foo {_x :: Int, _y :: Bool}
--   deriving Show
--
-- 'makeLensesWith' ('lensRules' & 'generateLazyPatterns' .~ True) ''Foo
-- @
--
-- @
-- > undefined & x .~ 8 & y .~ True
-- Foo {_x = 8, _y = True}
-- @
--
-- The downside of this flag is that it can lead to space-leaks and
-- code-size/compile-time increases when generated for large records. By default
-- this flag is turned off, and strict optics are generated.
--
-- When using lazy optics the strict optic can be recovered by composing with
-- 'equality'':
--
-- @
-- strictOptic = equality' % lazyOptic
-- @
generateLazyPatterns :: Lens' LensRules Bool
generateLazyPatterns = lensVL $ \f r ->
  fmap (\x -> r { _lazyPatterns = x}) (f (_lazyPatterns r))

-- | Create the class if the constructor if generated lenses would be
-- type-preserving and the 'lensClass' rule matches.
createClass :: Lens' LensRules Bool
createClass = lensVL $ \f r ->
  fmap (\x -> r { _generateClasses = x}) (f (_generateClasses r))

-- | 'Lens'' to access the convention for naming fields in our 'LensRules'.
lensField :: Lens' LensRules FieldNamer
lensField = lensVL $ \f r ->
  fmap (\x -> r { _fieldToDef = x}) (f (_fieldToDef r))

-- | 'Lens'' to access the option for naming "classy" lenses.
lensClass :: Lens' LensRules ClassyNamer
lensClass = lensVL $ \f r ->
  fmap (\x -> r { _classyLenses = x }) (f (_classyLenses r))

----------------------------------------
-- Common sets of rules

-- | Field rules for fields without any prefix. Useful for generation of field
-- labels when paired with @DuplicateRecordFields@ language extension so that no
-- prefixes for field names are necessary.
--
-- @since 0.2
noPrefixFieldLabels :: LensRules
noPrefixFieldLabels = fieldLabelsRules { _fieldToDef = noPrefixNamer }

abbreviatedFieldLabels :: LensRules
abbreviatedFieldLabels = fieldLabelsRules { _fieldToDef = abbreviatedNamer }

-- | Field rules for fields in the form @ _prefix_fieldname @
underscoreFields :: LensRules
underscoreFields = defaultFieldRules & lensField .~ underscoreNamer

-- | Field rules for fields in the form @ prefixFieldname or _prefixFieldname @
--
-- If you want all fields to be lensed, then there is no reason to use an @_@
-- before the prefix.  If any of the record fields leads with an @_@ then it is
-- assume a field without an @_@ should not have a lens created.
--
-- __Note__: The @prefix@ must be the same as the typename (with the first
-- letter lowercased). This is a change from lens versions before lens 4.5. If
-- you want the old behaviour, use 'makeLensesWith' 'abbreviatedFields'
camelCaseFields :: LensRules
camelCaseFields = defaultFieldRules

-- | Field rules for fields in the form @ _fieldname @ (the leading
-- underscore is mandatory).
--
-- __Note__: The primary difference to 'camelCaseFields' is that for
-- @classUnderscoreNoPrefixFields@ the field names are not expected to
-- be prefixed with the type name. This might be the desired behaviour
-- when the @DuplicateRecordFields@ extension is enabled.
classUnderscoreNoPrefixFields :: LensRules
classUnderscoreNoPrefixFields =
  defaultFieldRules & lensField .~ classUnderscoreNoPrefixNamer

-- | Field rules fields in the form @ prefixFieldname or _prefixFieldname @
-- If you want all fields to be lensed, then there is no reason to use an @_@ before the prefix.
-- If any of the record fields leads with an @_@ then it is assume a field without an @_@ should not have a lens created.
--
-- Note that @prefix@ may be any string of characters that are not uppercase
-- letters. (In particular, it may be arbitrary string of lowercase letters
-- and numbers) This is the behavior that 'defaultFieldRules' had in lens
-- 4.4 and earlier.
abbreviatedFields :: LensRules
abbreviatedFields = defaultFieldRules { _fieldToDef = abbreviatedNamer }

----------------------------------------
-- Namers

-- | A 'FieldNamer' that leaves the field name as-is. Useful for generation of
-- field labels when paired with @DuplicateRecordFields@ language extension so
-- that no prefixes for field names are necessary.
--
-- @since 0.2
noPrefixNamer :: FieldNamer
noPrefixNamer _ _ n = [TopName n]

-- | A 'FieldNamer' that strips the _ off of the field name, lowercases the
-- name, and skips the field if it doesn't start with an '_'.
underscoreNoPrefixNamer :: FieldNamer
underscoreNoPrefixNamer _ _ n =
  case nameBase n of
    '_':x:xs -> [TopName (mkName (toLower x:xs))]
    _        -> []


-- | Create a 'FieldNamer' from explicit pairings of @(fieldName, lensName)@.
lookingupNamer :: [(String,String)] -> FieldNamer
lookingupNamer kvs _ _ field =
  [ TopName (mkName v) | (k,v) <- kvs, k == nameBase field]

-- | Create a 'FieldNamer' from a mapping function. If the function returns
-- @[]@, it creates no lens for the field.
mappingNamer :: (String -> [String]) -- ^ A function that maps a @fieldName@ to
                                     -- @lensName@s.
             -> FieldNamer
mappingNamer mapper _ _ = fmap (TopName . mkName) . mapper . nameBase

-- | A 'FieldNamer' for 'underscoreFields'.
underscoreNamer :: FieldNamer
underscoreNamer _ _ field = maybeToList $ do
  _      <- prefix field'
  method <- niceLens
  cls    <- classNaming
  return (MethodName (mkName cls) (mkName method))
  where
    field' = nameBase field
    prefix ('_':xs) | '_' `List.elem` xs = Just (takeWhile (/= '_') xs)
    prefix _                             = Nothing
    niceLens    = prefix field' <&> \n -> drop (length n + 2) field'
    classNaming = niceLens <&> ("Has_" ++)

-- | A 'FieldNamer' for 'camelCaseFields'.
camelCaseNamer :: FieldNamer
camelCaseNamer tyName fields field = maybeToList $ do

  fieldPart <- stripPrefix expectedPrefix (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  expectedPrefix = optUnderscore ++ over _head toLower (nameBase tyName)

  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing

-- | A 'FieldNamer' for 'classUnderscoreNoPrefixFields'.
classUnderscoreNoPrefixNamer :: FieldNamer
classUnderscoreNoPrefixNamer _ _ field = maybeToList $ do
  fieldUnprefixed <- stripPrefix "_" (nameBase field)
  let className  = "Has" ++ over _head toUpper fieldUnprefixed
      methodName = fieldUnprefixed
  return (MethodName (mkName className) (mkName methodName))

-- | A 'FieldNamer' for 'abbreviatedFields'.
abbreviatedNamer :: FieldNamer
abbreviatedNamer _ fields field = maybeToList $ do

  fieldPart <- stripMaxLc (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  stripMaxLc f = do x <- stripPrefix optUnderscore f
                    case break isUpper x of
                      (p,s) | List.null p || List.null s -> Nothing
                            | otherwise                  -> Just s
  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing

----------------------------------------
-- Internal TH Implementation

-- Declaration quote stuff

declareWith :: (Dec -> Declare Dec) -> DecsQ -> DecsQ
declareWith fun = (runDeclare . traverseDataAndNewtype fun =<<)

-- | Monad for emitting top-level declarations as a side effect. We also track
-- the set of field class 'Name's that have been created and consult them to
-- avoid creating duplicate classes.

-- See #463 for more information.
type Declare = WriterT (Endo [Dec]) (StateT (Set Name) Q)

liftDeclare :: Q a -> Declare a
liftDeclare = lift . lift

runDeclare :: Declare [Dec] -> DecsQ
runDeclare dec = do
  (out, endo) <- evalStateT (runWriterT dec) Set.empty
  return $ out ++ appEndo endo []

emit :: [Dec] -> Declare ()
emit decs = tell $ Endo (decs++)

-- | Traverse each data, newtype, data instance or newtype instance
-- declaration.
traverseDataAndNewtype :: (Applicative f) => (Dec -> f Dec) -> [Dec] -> f [Dec]
traverseDataAndNewtype f decs = traverse go decs
  where
    go dec = case dec of
      DataD{} -> f dec
      NewtypeD{} -> f dec
      DataInstD{} -> f dec
      NewtypeInstD{} -> f dec

      -- Recurse into instance declarations because they main contain
      -- associated data family instances.
      InstanceD moverlap ctx inst body -> InstanceD moverlap ctx inst <$> traverse go body
      _ -> pure dec

stripFields :: Dec -> Dec
stripFields dec = case dec of
  DataD ctx tyName tyArgs kind cons derivings ->
    DataD ctx tyName tyArgs kind (map deRecord cons) derivings
  NewtypeD ctx tyName tyArgs kind con derivings ->
    NewtypeD ctx tyName tyArgs kind (deRecord con) derivings
  DataInstD ctx tyName tyArgs kind cons derivings ->
    DataInstD ctx tyName tyArgs kind (map deRecord cons) derivings
  NewtypeInstD ctx tyName tyArgs kind con derivings ->
    NewtypeInstD ctx tyName tyArgs kind (deRecord con) derivings
  _ -> dec

deRecord :: Con -> Con
deRecord con@NormalC{} = con
deRecord con@InfixC{} = con
deRecord (ForallC tyVars ctx con) = ForallC tyVars ctx $ deRecord con
deRecord (RecC conName fields) = NormalC conName (map dropFieldName fields)
deRecord con@GadtC{} = con
deRecord (RecGadtC ns fields retTy) = GadtC ns (map dropFieldName fields) retTy

dropFieldName :: VarBangType -> BangType
dropFieldName (_, str, typ) = (str, typ)
