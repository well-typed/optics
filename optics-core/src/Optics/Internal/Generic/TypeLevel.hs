{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module is intended for internal use only, and may change without
-- warning in subsequent releases.
module Optics.Internal.Generic.TypeLevel
  ( -- * Pathing
    PathTree(..)
  , Path(..)
  -- ** Names
  , GetFieldPaths
  , GetFieldPath
  , GetConstructorPath
  -- ** Positions
  , GetPositionPaths
  , GetPositionPath
  -- * Misc
  , AnyHasPath
  , NoGenericError
  ) where

import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

import Optics.Internal.Optic.TypeLevel

-- | A map that allows reaching a specific field in a generic representation of
-- a data type. Computed up front by generic optics for early error reporting
-- and efficient data traversal.
data PathTree e
  = PathNode (PathTree e) (PathTree e)
  | PathLeaf (Either e [Path])

data Path = PathLeft | PathRight

----------------------------------------
-- Paths to a field

-- | Compute paths to a field with a specific name.
type family GetFieldPaths s (name :: Symbol) g :: PathTree Symbol where
  GetFieldPaths s name (M1 D _ g)  = GetFieldPaths s name g
  GetFieldPaths s name (g1 :+: g2) = PathNode (GetFieldPaths s name g1)
                                              (GetFieldPaths s name g2)
  GetFieldPaths s name (M1 C _ g)  = PathLeaf (GetFieldPath name g)

  GetFieldPaths s name V1 = TypeError (Text "Type " :<>: QuoteType s :<>:
                                       Text " has no data constructors")

-- Each reduction step of the type families below is a part of the evidence,
-- so compilation time depends on the size of their arguments and on the
-- number of their equations. Hence, the paths are built on the way up instead
-- of in an accumulator and the searches through sums and products are separate.

-- | Compute path to a field in a product with a specific name.
type family GetFieldPath (name :: Symbol) g :: Either Symbol [Path] where
  GetFieldPath name (M1 S (MetaSel (Just name) _ _ _) _) = Right '[]
  GetFieldPath name (g1 :*: g2) = ChoosePath (GetFieldPath name g1)
                                             (GetFieldPath name g2)
  GetFieldPath name _ = Left name

-- | Compute path to a constructor in a sum with a specific name.
type family GetConstructorPath (name :: Symbol) g :: Either Symbol [Path] where
  GetConstructorPath name (M1 D _ g) = GetConstructorPath name g
  GetConstructorPath name (M1 C (MetaCons name _ _) _) = Right '[]
  GetConstructorPath name (g1 :+: g2) = ChoosePath (GetConstructorPath name g1)
                                                   (GetConstructorPath name g2)
  GetConstructorPath name _ = Left name

-- | Extend the path found in the left branch, otherwise the one found in the
-- right branch.
type family ChoosePath (l :: Either Symbol [Path]) (r :: Either Symbol [Path])
  :: Either Symbol [Path] where
  ChoosePath (Right path) _            = Right (PathLeft : path)
  ChoosePath _            (Right path) = Right (PathRight : path)
  ChoosePath l            _            = l

----------------------------------------
-- Paths to a position

-- | Compute paths to a field at a specific position.
type family GetPositionPaths s (pos :: Nat) g :: PathTree (Nat, Nat) where
  GetPositionPaths s pos (M1 D _ g)  = GetPositionPaths s pos g
  GetPositionPaths s pos (g1 :+: g2) = PathNode (GetPositionPaths s pos g1)
                                                (GetPositionPaths s pos g2)
  GetPositionPaths s pos (M1 C _ g)  = PathLeaf (GetPositionPath pos g 0)

  GetPositionPaths s pos V1 = TypeError (Text "Type " :<>: QuoteType s :<>:
                                         Text " has no data constructors")

-- | Compute path to a field in a product at a specific position, given the
-- number of fields preceding the product.
type family GetPositionPath (pos :: Nat) g (k :: Nat)
  :: Either (Nat, Nat) [Path] where
  GetPositionPath pos (M1 S _ _) k =
    If (pos == k + 1) (Right '[]) (Left '(pos, k + 1))
  GetPositionPath pos (g1 :*: g2) k =
    ContinueWhenLeft (GetPositionPath pos g1 k) g2

  -- The second element is the number of fields in the data constructor.
  GetPositionPath pos _ k = Left '(pos, k)

-- | If the left branch had the position we're looking for, return it. Otherwise
-- continue with the right branch.
type family ContinueWhenLeft (r :: Either (Nat, Nat) [Path]) g
  :: Either (Nat, Nat) [Path] where
  ContinueWhenLeft (Right path)     _ = Right (PathLeft : path)
  ContinueWhenLeft (Left '(pos, k)) g = PrependRight (GetPositionPath pos g k)

type family PrependRight (r :: Either (Nat, Nat) [Path])
  :: Either (Nat, Nat) [Path] where
  PrependRight (Right path) = Right (PathRight : path)
  PrependRight l            = l

----------------------------------------
-- Misc

-- | Check if any leaf in the tree has a '[Path]'.
type family AnyHasPath (path :: PathTree e) :: Bool where
  AnyHasPath (PathNode path1 path2) = AnyHasPath path1 || AnyHasPath path2
  AnyHasPath (PathLeaf (Right _))  = True
  AnyHasPath (PathLeaf (Left _ ))  = False

type family NoGenericError t where
  NoGenericError t = TypeError
    (Text "Type " :<>: QuoteType t :<>:
     Text " doesn't have a Generic instance")
