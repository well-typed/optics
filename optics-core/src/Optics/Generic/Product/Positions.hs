{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UndecidableInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Generic.Product.Positions
-- Copyright   :  (C) 2018 Csongor Kiss
-- License     :  BSD3
-- Maintainer  :  Csongor Kiss <kiss.csongor.kiss@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Derive positional product type getters and setters generically.
--
-----------------------------------------------------------------------------

module Optics.Generic.Product.Positions
  ( -- *Lenses

    -- $setup
    HasPosition (..)
  , HasPosition' (..)
  , HasPosition_ (..)
  , HasPosition0 (..)
  ) where

import Data.Kind      (Constraint, Type)
import Data.Type.Bool (type (&&))
import Data.Coerce
import GHC.Generics
import GHC.TypeLits   (type (<=?),  Nat, TypeError, ErrorMessage(..))

import Optics.Internal.Generic.Void
import Optics.Internal.Generic.Families
import Optics.Internal.Generic.Product.Positions
import Optics.Internal.Generic.Product.GLens
import Optics.Internal.Generic.Errors
import Optics.Internal.Generic.Utils
import Optics.Iso
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
-- data Human = Human
--   { name    :: String
--   , age     :: Int
--   , address :: String
--   }
--   deriving (Generic, Show)
-- human :: Human
-- human = Human "Tunyasz" 50 "London"
-- :}

-- |Records that have a field at a given position.
class HasPosition (i :: Nat) s t a b | s i -> a, t i -> b, s i b -> t, t i a -> s where
  -- |A lens that focuses on a field at a given position.
  --
  --  >>> human ^. position @1
  --  "Tunyasz"
  --  >>> human & position @3 .~ "Berlin"
  --  Human {name = "Tunyasz", age = 50, address = "Berlin"}
  --
  --  === /Type errors/
  --
  --  >>> human & position @4 .~ "Berlin"
  --  ...
  --  ... The type Human does not contain a field at position 4
  --  ...
  position :: Lens s t a b

class HasPosition_ (i :: Nat) s t a b where
  position_ :: Lens s t a b

-- |Records that have a field at a given position.
--
-- The difference between 'HasPosition' and 'HasPosition_' is similar to the
-- one between 'Optics.Generic.Product.Fields.HasField' and
-- 'Optics.Generic.Product.Fields.HasField_'.
-- See 'Optics.Generic.Product.Fields.HasField_'.
class HasPosition' (i :: Nat) s a | s i -> a where
  position' :: Lens s s a a

-- |Records that have a field at a given position.
--
-- This class gives the minimal constraints needed to define this lens.
-- For common uses, see 'HasPosition'.
class HasPosition0 (i :: Nat) s t a b where
  position0 :: Lens s t a b

instance
  ( Generic s
  , ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , cs ~ CRep s
  , Coercible (Rep s) cs
  , GLens' (HasTotalPositionPSym i) cs a
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic lens focusing on the field at"
                  , 'Text "position " ':<>: QuoteType i ':<>: 'Text " of type " ':<>: QuoteType a
                    ':<>: 'Text " in " ':<>: QuoteType s
                  ])
    (() :: Constraint)
  ) => HasPosition' i s a where
  position' = genericLens $ coerced_ @cs @cs % glens @(HasTotalPositionPSym i)
  {-# INLINE position' #-}

-- this is to 'hide' the equality constraints which interfere with inlining
-- pre 8.4.3
class (~~) (a :: k) (b :: k) | a -> b, b -> a
instance (a ~ b) => (~~) a b

instance  -- see Note [Changing type parameters]
  ( ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , HasTotalPositionP i (CRep s) ~~ 'Just a
  , HasTotalPositionP i (CRep t) ~~ 'Just b
  , HasTotalPositionP i (CRep (Indexed s)) ~~ 'Just a'
  , HasTotalPositionP i (CRep (Indexed t)) ~~ 'Just b'
  , t ~~ Infer s a' b
  , s ~~ Infer t b' a
  , Coercible (CRep s) (Rep s)
  , Coercible (CRep t) (Rep t)
  , HasPosition0 i s t a b
  ) => HasPosition i s t a b where

  position = position0 @i
  {-# INLINE position #-}

-- See Note [Uncluttering type signatures]
instance {-# OVERLAPPING #-} HasPosition f (Void1 a) (Void1 b) a b where
  position = undefined

instance
  ( ErrorUnless i s (0 <? i && i <=? Size (Rep s))
  , UnifyHead s t
  , UnifyHead t s
  , Coercible (CRep s) (Rep s)
  , Coercible (CRep t) (Rep t)
  , HasPosition0 i s t a b
  ) => HasPosition_ i s t a b where

  position_ = position0 @i
  {-# INLINE position_ #-}

instance {-# OVERLAPPING #-} HasPosition_ f (Void1 a) (Void1 b) a b where
  position_ = undefined

instance
  ( Generic s
  , Generic t
  , GLens (HasTotalPositionPSym i) (CRep s) (CRep t) a b
  , Coercible (CRep s) (Rep s)
  , Coercible (CRep t) (Rep t)
  , Defined (Rep s)
    (NoGeneric s '[ 'Text "arising from a generic lens focusing on the field at"
                  , 'Text "position " ':<>: QuoteType i ':<>: 'Text " of type " ':<>: QuoteType a
                    ':<>: 'Text " in " ':<>: QuoteType s
                  ])
    (() :: Constraint)
  ) => HasPosition0 i s t a b where
  position0 = genericLens $ coerced_ @(CRep s) @(CRep t) % glens @(HasTotalPositionPSym i)
  {-# INLINE position0 #-}

type family ErrorUnless (i :: Nat) (s :: Type) (hasP :: Bool) :: Constraint where
  ErrorUnless i s 'False
    = TypeError
        (     'Text "The type "
        ':<>: 'ShowType s
        ':<>: 'Text " does not contain a field at position "
        ':<>: 'ShowType i
        )

  ErrorUnless _ _ 'True
    = ()

data HasTotalPositionPSym  :: Nat -> (TyFun (Type -> Type) (Maybe Type))
type instance Eval (HasTotalPositionPSym t) tt = HasTotalPositionP t tt

-- We wouldn't need the universal 'a' here if we could express above that forall
-- a. Coercible (cs a) (Rep s a), but this requires quantified constraints.
coerced_
  :: forall f g f' g' a. (Coercible f f', Coercible g g')
  => Iso (f' a) (g' a) (f a) (g a)
coerced_ = coerced
{-# INLINE coerced_ #-}
