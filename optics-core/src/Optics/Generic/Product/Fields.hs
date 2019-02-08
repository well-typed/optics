{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Generic.Product.Fields
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive record field getters and setters generically.
--
-----------------------------------------------------------------------------

module Optics.Generic.Product.Fields
  ( -- *Lenses

    -- $setup
    HasField (..)
  , HasField' (..)
  , HasField_ (..)
  , HasField0 (..)
  ) where


import Data.Kind    (Constraint, Type)
import GHC.Generics
import GHC.TypeLits (Symbol, ErrorMessage(..), TypeError)

import Optics.Internal.Generic.Errors
import Optics.Internal.Generic.Families
import Optics.Internal.Generic.Void
import Optics.Internal.Generic.Utils
import Optics.Internal.Generic.Product.GLens
import Optics.Lens

-- $setup
-- == /Running example:/
--
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> import GHC.Generics
-- >>> :m +Optics.Operators
-- >>> :m +Data.Function
-- >>> :{
-- data Human a
--   = Human
--     { name    :: String
--     , age     :: Int
--     , address :: String
--     , other   :: a
--     }
--   | HumanNoAddress
--     { name    :: String
--     , age     :: Int
--     , other   :: a
--     }
--   deriving (Generic, Show)
-- human :: Human Bool
-- human = Human { name = "Tunyasz", age = 50, address = "London", other = False }
-- :}

-- |Records that have a field with a given name.
class HasField (field :: Symbol) s t a b | s field -> a, t field -> b, s field b -> t, t field a -> s where
  -- |A lens that focuses on a field with a given name.
  --
  --  >>> human ^. field @"age"
  --  50
  --
  --  === /Type changing/
  --
  --  >>> :t human
  --  human :: Human Bool
  --
  --  >>> :t human & field @"other" .~ (42 :: Int)
  --  human & field @"other" .~ (42 :: Int) :: Human Int
  --
  --  >>> human & field @"other" .~ 42
  --  Human {name = "Tunyasz", age = 50, address = "London", other = 42}
  --
  --  === /Type errors/
  --
  --  >>> human & field @"weight" .~ 42
  --  ...
  --  ... The type Human Bool does not contain a field named 'weight'.
  --  ...
  --
  --  >>> human & field @"address" .~ ""
  --  ...
  --  ... Not all constructors of the type Human Bool
  --  ... contain a field named 'address'.
  --  ... The offending constructors are:
  --  ... HumanNoAddress
  --  ...
  field :: Lens s t a b

-- |Records that have a field with a given name.
--
-- This is meant to be more general than 'HasField', but that is not quite the
-- case due to the lack of functional dependencies.
--
-- The types @s@ and @t@ must be applications of the same type constructor.
-- In contrast, 'HasField' also requires the parameters of that type constructor
-- to have representational roles.
--
-- One use case of 'HasField_' over 'HasField' is for records defined with
-- @data instance@.
class HasField_ (field :: Symbol) s t a b where
  field_ :: Lens s t a b

class HasField' (field :: Symbol) s a | s field -> a where
  field' :: Lens s s a a

-- |Records that have a field with a given name.
--
-- This class gives the minimal constraints needed to define this lens.
-- For common uses, see 'HasField'.
class HasField0 (field :: Symbol) s t a b where
  field0 :: Lens s t a b

instance
  ( Generic s
  , ErrorUnless field s (CollectField field (Rep s))
  , GLens' (HasTotalFieldPSym field) (Rep s) a
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic lens focusing on the "
                    ':<>: QuoteType field ':<>: 'Text " field of type " ':<>: QuoteType a
                  , 'Text "in " ':<>: QuoteType s])
    (() :: Constraint)
  ) => HasField' field s a where
  field' = field0 @field
  {-# INLINE field' #-}

class (~~) (a :: k) (b :: k) | a -> b, b -> a
instance (a ~ b) => (~~) a b

instance  -- see Note [Changing type parameters]
  ( HasTotalFieldP field (Rep s) ~~ 'Just a
  , HasTotalFieldP field (Rep t) ~~ 'Just b
  , HasTotalFieldP field (Rep (Indexed s)) ~~ 'Just a'
  , HasTotalFieldP field (Rep (Indexed t)) ~~ 'Just b'
  , t ~~ Infer s a' b
  , s ~~ Infer t b' a
  , HasField0 field s t a b
  ) => HasField field s t a b where
  field = field0 @field
  {-# INLINE field #-}

-- -- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} HasField f (Void1 a) (Void1 b) a b where
  field = undefined

instance
  ( HasTotalFieldP field (Rep s) ~~ 'Just a
  , HasTotalFieldP field (Rep t) ~~ 'Just b
  , UnifyHead s t
  , UnifyHead t s
  , HasField0 field s t a b
  ) => HasField_ field s t a b where
  field_ = field0 @field
  {-# INLINE field_ #-}

instance {-# OVERLAPPING #-} HasField_ f (Void1 a) (Void1 b) a b where
  field_ = undefined

instance
  ( Generic s
  , Generic t
  , GLens  (HasTotalFieldPSym field) (Rep s) (Rep t) a b
  , ErrorUnless field s (CollectField field (Rep s))
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic lens focusing on the "
                    ':<>: QuoteType field ':<>: 'Text " field of type " ':<>: QuoteType a
                  , 'Text "in " ':<>: QuoteType s])
    (() :: Constraint)
  ) => HasField0 field s t a b where
  field0 = genericLens $ glens @(HasTotalFieldPSym field)
  {-# INLINE field0 #-}

type family ErrorUnless (field :: Symbol) (s :: Type) (stat :: TypeStat) :: Constraint where
  ErrorUnless field s ('TypeStat _ _ '[])
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field named '"
        ':<>: 'Text field ':<>: 'Text "'."
        )

  ErrorUnless field s ('TypeStat (n ': ns) _ _)
    = TypeError
        (     'Text "Not all constructors of the type "
        ':<>: 'ShowType s
        ':$$: 'Text " contain a field named '"
        ':<>: 'Text field ':<>: 'Text "'."
        ':$$: 'Text "The offending constructors are:"
        ':$$: ShowSymbols (n ': ns)
        )

  ErrorUnless _ _ ('TypeStat '[] '[] _)
    = ()

data HasTotalFieldPSym :: Symbol -> (TyFun (Type -> Type) (Maybe Type))
type instance Eval (HasTotalFieldPSym sym) tt = HasTotalFieldP sym tt
