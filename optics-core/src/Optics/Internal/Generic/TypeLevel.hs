{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Generic.TypeLevel where

import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

import Optics.Internal.Optic.TypeLevel

-- | A map that allows reaching a specific field in a generic representation of
-- a data type. Computed up front by generic optics for early error reporting
-- and efficient data traversal.
data PathTree e
  = PathTree (PathTree e) (PathTree e)
  | PathLeaf (Either e [Path])

data Path = PathLeft | PathRight

----------------------------------------
-- Paths to a field

-- | Compute paths to a field with a specific name.
type family GetFieldPaths s (name :: Symbol) g :: PathTree Symbol where
  GetFieldPaths s name (M1 D _ g)  = GetFieldPaths s name g
  GetFieldPaths s name (g1 :+: g2) = 'PathTree (GetFieldPaths s name g1)
                                               (GetFieldPaths s name g2)
  GetFieldPaths s name (M1 C _ g)  = 'PathLeaf (GetNamePath name g '[])

  GetFieldPaths s name V1 = TypeError ('Text "Type " ':<>: QuoteType s ':<>:
                                       'Text " has no data constructors")

-- | Compute path to a constructor in a sum or a field in a product with a
-- specific name.
type family GetNamePath (name :: Symbol) g (acc :: [Path]) :: Either Symbol [Path] where
  GetNamePath name (M1 D _ g) acc = GetNamePath name g acc

  -- Find path to a constructor in a sum type
  GetNamePath name (M1 C ('MetaCons name _ _) _) acc = 'Right (Reverse acc '[])
  GetNamePath name (g1 :+: g2) acc = FirstRight (GetNamePath name g1 ('PathLeft  : acc))
                                                (GetNamePath name g2 ('PathRight : acc))

  -- Find path to a field in a product type
  GetNamePath name (M1 S ('MetaSel ('Just name) _ _ _) _) acc = 'Right (Reverse acc '[])
  GetNamePath name (g1 :*: g2) acc = FirstRight (GetNamePath name g1 ('PathLeft  : acc))
                                                (GetNamePath name g2 ('PathRight : acc))

  GetNamePath name _ _ = 'Left name

----------------------------------------
-- Paths to a position

-- | Compute paths to a field at a specific position.
type family GetPositionPaths s (n :: Nat) g :: PathTree (Nat, Nat) where
  GetPositionPaths s n (M1 D _ g)  = GetPositionPaths s n g
  GetPositionPaths s n (g1 :+: g2) = 'PathTree (GetPositionPaths s n g1)
                                               (GetPositionPaths s n g2)
  GetPositionPaths s n (M1 C _ g)  = 'PathLeaf (GetPositionPath n g 1 '[])

  GetPositionPaths s n V1 = TypeError ('Text "Type " ':<>: QuoteType s ':<>:
                                       'Text " has no data constructors")

-- | Compute path to a constructor in a sum or a field in a product at a
-- specific position.
type family GetPositionPath (n :: Nat) g (k :: Nat) (acc :: [Path])
  :: Either (Nat, Nat) [Path] where
  GetPositionPath n (M1 D _ g) k acc = GetPositionPath n g k acc

  -- Find field at a position in a sum type
  GetPositionPath n (M1 C _ _) k acc =
    If (n == k) ('Right (Reverse acc '[])) ('Left '(n, k))
  GetPositionPath n (g1 :+: g2) k acc =
    ContinueWhenLeft (GetPositionPath n g1 k ('PathLeft : acc)) g2 acc

  -- Find field at a position in a product type
  GetPositionPath n (M1 S _ _) k acc =
    If (n == k) ('Right (Reverse acc '[])) ('Left '(n, k))
  GetPositionPath n (g1 :*: g2) k acc =
    ContinueWhenLeft (GetPositionPath n g1 k ('PathLeft : acc)) g2 acc

  GetPositionPath n _ k _ = 'Left '(n, k)

-- | If the left branch had the position we're looking for, return it. Otherwise
-- continue with the right branch.
type family ContinueWhenLeft (r :: Either (Nat, Nat) [Path]) g acc
  :: Either (Nat, Nat) [Path] where
  ContinueWhenLeft ('Right path)   _ _   = 'Right path
  ContinueWhenLeft ('Left '(n, k)) g acc = GetPositionPath n g (k + 1) ('PathRight : acc)

----------------------------------------
-- Misc

-- | Check if any leaf in the tree has a '[Path]'.
type family AnyHasPath (path :: PathTree e) :: Bool where
  AnyHasPath ('PathTree path1 path2) = AnyHasPath path1 || AnyHasPath path2
  AnyHasPath ('PathLeaf ('Right _))  = 'True
  AnyHasPath ('PathLeaf ('Left _ ))  = 'False

type family NoGenericError t where
  NoGenericError t = TypeError
     ('Text "Type " ':<>: QuoteType t ':<>:
      'Text " doesn't have a Generic instance")
