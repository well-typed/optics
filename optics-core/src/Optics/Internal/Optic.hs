{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Core optic types and subtyping machinery.
--
-- This module contains the core 'Optic' types, and the underlying
-- machinery that we need in order to implement the subtyping between
-- various different flavours of optics.
--
-- The composition operator for optics is also defined here.
--
-- This module is intended for internal use only, and may change without
-- warning in subsequent releases.
--
module Optics.Internal.Optic
  ( Optic(..)
  , Optic'
  , Optic_
  , Optic__
  , NoIx
  , WithIx
  , castOptic
  , (%)
  , (%%)
  , IsProxy(..)
  -- * Labels
  , LabelOptic(..)
  , LabelOptic'
  -- * Re-exports
  , module Optics.Internal.Optic.Subtyping
  , module Optics.Internal.Optic.Types
  , module Optics.Internal.Optic.TypeLevel
  ) where

import Data.Proxy (Proxy (..))
import Data.Type.Equality
import GHC.OverloadedLabels
import GHC.TypeLits

import Optics.Internal.Optic.Subtyping
import Optics.Internal.Optic.TypeLevel
import Optics.Internal.Optic.Types

-- to make %% simpler
import Unsafe.Coerce (unsafeCoerce)

-- | An alias for an empty index-list
type NoIx = '[]

-- | Singleton index list
type WithIx i = '[i]

-- | Wrapper newtype for the whole family of vaguely lens-like things.
--
-- The first parameter @k@ identifies the particular flavour
-- (e.g. 'A_Lens' or 'A_Traversal').
--
-- The parameter @is@ is a list of types available as indices.  This
-- will typically be 'NoIx' for unindexed optics, or 'WithIx' for
-- optics with a single index. See "Optics.Indexed.Core" for
-- discussion of indexed optics.
--
-- The parameters @s@ and @t@ represent the "big" structure,
-- whereas @a@ and @b@ represent the "small" structure.
--
newtype Optic (k :: *) (is :: [*]) s t a b =
  Optic { getOptic :: forall p j. Optic_ k p j (Curry is j) s t a b }

-- | Common special case of 'Optic' where source and target types are equal.
--
-- Here, we need only one "big" and one "small" type. For lenses, this
-- means that in the restricted form we cannot do type-changing updates.
--
type Optic' k is s a = Optic k is s s a a

-- | Type representing the various kinds of optics.
--
-- The tag parameter @k@ is translated into constraints on @p@
-- via the type family 'Constraints'.
--
type Optic_ k p i o s t a b = Constraints k p => Optic__ p i o s t a b

-- | Optic internally as a profunctor transformation.
type Optic__ p i o s t a b = p i a b -> p o s t

-- | Proxy type for use as an argument to 'implies'.
--
data IsProxy (k :: *) (l :: *) (p :: * -> * -> * -> *) =
  IsProxy

-- | Explicit cast from one optic flavour to another.
--
-- This is the identity function, modulo some constraint jiggery-pokery.
--
castOptic
  :: forall k l is s t a b
  .  Is k l
  => Optic k is s t a b
  -> Optic l is s t a b
castOptic (Optic o) = Optic (implies' o)
  where
    implies'
      :: forall p j
      .  Optic_ k p j (Curry is j) s t a b
      -> Optic_ l p j (Curry is j) s t a b
    implies' x = implies (IsProxy :: IsProxy k l p) x
{-# INLINE castOptic #-}

-- | Compose two optics of compatible flavours.
--
-- Returns an optic of the appropriate supertype.
--
infixr 9 %
(%) :: (Is k m, Is l m, m ~ Join k l, ks ~ Append is js)
    => Optic k is s t u v
    -> Optic l js u v a b
    -> Optic m ks s t a b
o % o' = castOptic o %% castOptic o'
{-# INLINE (%) #-}

-- | Compose two optics of the same flavour.
--
-- Normally you can simply use ('%') instead, but this may be useful to help
-- type inference if the type of one of the optics is otherwise
-- under-constrained.
infixr 9 %%
(%%) :: forall k is js ks s t u v a b. ks ~ Append is js
     => Optic k is s t u v
     -> Optic k js u v a b
     -> Optic k ks s t a b
Optic o %% Optic o' = Optic oo
  where
    -- unsafeCoerce to the rescue, for a proof see below.
    oo :: forall p j. Optic_ k p j (Curry ks j) s t a b
    oo = (unsafeCoerce
           :: Optic_ k p j (Curry is (Curry js j)) s t a b
           -> Optic_ k p i (Curry ks j           ) s t a b)
      (o . o')
{-# INLINE (%%) #-}

-- |
--
-- 'AppendProof' is a very simple class which provides a witness
--
-- @
-- foldr f (foldr f init xs) ys = foldr f init (ys ++ xs)
--    where f = (->)
-- @
--
-- It shows that usage of 'unsafeCoerce' in '(%%)' is, in fact, safe.
--
class Append xs ys ~ zs => AppendProof (xs :: [*]) (ys :: [*]) (zs :: [*])
  | xs ys -> zs, zs xs -> ys {- , zs ys -> xs -} where
  appendProof :: Proxy i -> Curry xs (Curry ys i) :~: Curry zs i

instance ys ~ zs => AppendProof '[] ys zs where
  appendProof _ = Refl

instance
  (Append (x : xs) ys ~ (x : zs), AppendProof xs ys zs
  ) => AppendProof (x ': xs) ys (x ': zs) where
  appendProof
    :: forall i. Proxy i
    -> Curry (x ': xs) (Curry ys i) :~: Curry (x ': zs) i
  appendProof i = case appendProof @xs @ys @zs i of
    Refl -> Refl

----------------------------------------
-- Labels

-- | Support for optics as overloaded labels.
--
-- /Note:/ Functional dependencies guarantee good type inference, but also
-- create limitations. We can split them into two groups:
--
-- - @name s -> k a@, @name t -> k b@
--
-- - @name s b -> t@, @name t a -> s@
--
-- The first group ensures that when we compose two optics, the middle type is
-- unambiguous. The consequence is that it's not possible to create label optics
-- with @a@ or @b@ referencing type variables not referenced in @s@ or @t@,
-- i.e. getters for fields of rank 2 type or reviews for constructors with
-- existentially quantified types inside.
--
-- The second group ensures that when we perform a chain of updates, the middle
-- type is unambiguous. The consequence is that it's not possible to define
-- label optics that:
--
-- - Modify phantom type parameters of type @s@ or @t@.
--
-- - Modify type parameters of type @s@ or @t@ if @a@ or @b@ contain ambiguous
--   applications of type families to these type parameters.
--
-- Overloaded labels are a solution to Haskell's namespace problem for records.
--
-- Consider the following:
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XOverloadedLabels
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :{
-- data Human = Human
--   { humanName :: String
--   , humanAge  :: Integer
--   , humanPets :: [Pet]
--   } deriving Show
-- instance (a ~ String, b ~ String) => LabelOptic "name" A_Lens Human Human a b where
--   labelOptic = lensVL $ \f s -> (\v -> s { humanName = v }) <$> f (humanName s)
-- instance (a ~ Integer, b ~ Integer) => LabelOptic "age" A_Lens Human Human a b where
--   labelOptic = lensVL $ \f s -> (\v -> s { humanAge = v }) <$> f (humanAge s)
-- instance (a ~ [Pet], b ~ [Pet]) => LabelOptic "pets" A_Lens Human Human a b where
--   labelOptic = lensVL $ \f s -> (\v -> s { humanPets = v }) <$> f (humanPets s)
-- data Pet
--   = Cat  { petName :: String, petAge :: Int, petLazy :: Bool }
--   | Fish { petName :: String, petAge :: Int }
--   deriving Show
-- instance (a ~ String, b ~ String) => LabelOptic "name" A_Lens Pet Pet a b where
--   labelOptic = lensVL $ \f s -> (\v -> s { petName = v }) <$> f (petName s)
-- instance (a ~ Int, b ~ Int) => LabelOptic "age" A_Lens Pet Pet a b where
--   labelOptic = lensVL $ \f s -> (\v -> s { petAge = v }) <$> f (petAge s)
-- instance (a ~ Bool, b ~ Bool) => LabelOptic "lazy" An_AffineTraversal Pet Pet a b where
--   labelOptic = atraversalVL $ \point f s -> case s of
--     Cat name age lazy -> (\lazy' -> Cat name age lazy') <$> f lazy
--     _                 -> point s
-- peter :: Human
-- peter = Human "Peter" 13 [ Fish "Goldie" 1
--                          , Cat  "Loopy"  3 False
--                          , Cat  "Sparky" 2 True
--                          ]
-- :}
--
-- Now we can ask for Peter's name:
--
-- >>> view #name peter
-- "Peter"
--
-- or for names of his pets:
--
-- >>> toListOf (#pets % folded % #name) peter
-- ["Goldie","Loopy","Sparky"]
--
-- We can check whether any of his pets is lazy:
--
-- >>> orOf (#pets % folded % #lazy) peter
-- True
--
-- or how things might be be a year from now:
--
-- >>> peter & over #age (+1) & over (#pets % mapped % #age) (+1)
-- Human {humanName = "Peter", humanAge = 14, humanPets = [Fish {petName = "Goldie", petAge = 2},Cat {petName = "Loopy", petAge = 4, petLazy = False},Cat {petName = "Sparky", petAge = 3, petLazy = True}]}
--
-- Perhaps Peter is going on vacation and needs to leave his pets at home:
--
-- >>> peter & set #pets []
-- Human {humanName = "Peter", humanAge = 13, humanPets = []}
--
-- /Note:/ You might wonder why instances above are written in form
--
-- @
-- instance (a ~ [Pet], b ~ [Pet]) => LabelOptic "pets" A_Lens Human Human a b where
-- @
--
-- instead of
--
-- @
-- instance LabelOptic "pets" A_Lens Human Human [Pet] [Pet] where
-- @
--
-- The reason is that using the first form ensures that GHC always matches on
-- the instance if either @s@ or @t@ is known and verifies type equalities
-- later, which not only makes type inference better, but also allows it to
-- generate good error messages.
--
-- For example, if you try to write @peter & set #pets []@ with the appropriate
-- LabelOptic instance in the second form, you get the following:
--
-- @
-- <interactive>:16:1: error:
--    • No instance for LabelOptic "pets" ‘A_Lens’ ‘Human’ ‘()’ ‘[Pet]’ ‘[a0]’
--        (maybe you forgot to define it or misspelled a name?)
--    • In the first argument of ‘print’, namely ‘it’
--      In a stmt of an interactive GHCi command: print it
-- @
--
-- That's because empty list doesn't have type @[Pet]@, it has type @[r]@ and
-- GHC doesn't have enough information to match on the instance we
-- provided. We'd need to either annotate the list: @peter & set #pets
-- ([]::[Pet])@ or the result type: @peter & set #pets [] :: Human@, which is
-- suboptimal.
--
-- Here are more examples of confusing error messages if the instance for
-- @LabelOptic "age"@ is written without type equalities:
--
-- @
-- >>> view #age peter :: Char
--
-- <interactive>:28:6: error:
--     • No instance for LabelOptic "age" ‘k0’ ‘Human’ ‘Human’ ‘Char’ ‘Char’
--         (maybe you forgot to define it or misspelled a name?)
--     • In the first argument of ‘view’, namely ‘#age’
--       In the expression: view #age peter :: Char
--       In an equation for ‘it’: it = view #age peter :: Char
-- >>> peter & set #age "hi"
--
-- <interactive>:29:1: error:
--     • No instance for LabelOptic "age" ‘k’ ‘Human’ ‘b’ ‘a’ ‘[Char]’
--         (maybe you forgot to define it or misspelled a name?)
--     • When checking the inferred type
--         it :: forall k b a. ((TypeError ...), Is k A_Setter) => b
-- @
--
-- If we use the first form, error messages become more accurate:
--
-- @
-- >>> view #age peter :: Char
--
-- <interactive>:31:6: error:
--     • Couldn't match type ‘Char’ with ‘Integer’
--         arising from the overloaded label ‘#age’
--     • In the first argument of ‘view’, namely ‘#age’
--       In the expression: view #age peter :: Char
--       In an equation for ‘it’: it = view #age peter :: Char
-- >>> peter & set #age "hi"
--
-- <interactive>:32:13: error:
--     • Couldn't match type ‘[Char]’ with ‘Integer’
--         arising from the overloaded label ‘#age’
--     • In the first argument of ‘set’, namely ‘#age’
--       In the second argument of ‘(&)’, namely ‘set #age "hi"’
--       In the expression: peter & set #age "hi"
-- @
--
class LabelOptic (name :: Symbol) k s t a b | name s -> k a
                                            , name t -> k b
                                            , name s b -> t
                                            , name t a -> s where
  labelOptic :: Optic k NoIx s t a b

-- | If no instance matches, GHC tends to bury error messages "No instance for
-- LabelOptic..." within a ton of other error messages about ambiguous type
-- variables and overlapping instances which are irrelevant and confusing. Use
-- overlappable instance providing a custom type error to cut its efforts short.
instance {-# OVERLAPPABLE #-}
  (LabelOptic name k s t a b,
   TypeError
   ('Text "No instance for LabelOptic " ':<>: 'ShowType name
    ':<>: 'Text " " ':<>: QuoteType k
    ':<>: 'Text " " ':<>: QuoteType s
    ':<>: 'Text " " ':<>: QuoteType t
    ':<>: 'Text " " ':<>: QuoteType a
    ':<>: 'Text " " ':<>: QuoteType b
    ':$$: 'Text "  (maybe you forgot to define it or misspelled a name?)")
  ) => LabelOptic name k s t a b where
  labelOptic = error "unreachable"

-- | Type synonym for a type-preserving optic as overloaded label.
type LabelOptic' name k s a = LabelOptic name k s s a a

instance
  (LabelOptic name k s t a b, is ~ NoIx
  ) => IsLabel name (Optic k is s t a b) where
#if __GLASGOW_HASKELL__ >= 802
  fromLabel = labelOptic @name @k @s @t @a @b
#else
  fromLabel _ = labelOptic @name @k @s @t @a @b
#endif
