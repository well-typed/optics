{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Optics.Internal.TH
-- Copyright   :  (C) 2013-2016 Edward Kmett and Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Optics.Internal.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Set as Set

#ifndef CURRENT_PACKAGE_KEY
import Data.Version (showVersion)
import Paths_lens (version)
#endif

import Optics.Core

-- | Compatibility shim for recent changes to template haskell's 'tySynInstD'
tySynInstD' :: Name -> [TypeQ] -> TypeQ -> DecQ
#if MIN_VERSION_template_haskell(2,9,0)
tySynInstD' fam ts r = tySynInstD fam (tySynEqn ts r)
#else
tySynInstD' = tySynInstD
#endif

-- | Apply arguments to a type constructor
appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT

-- | Apply arguments to a function
appsE1 :: ExpQ -> [ExpQ] -> ExpQ
appsE1 = foldl appE

-- | Construct a tuple type given a list of types.
toTupleT :: [TypeQ] -> TypeQ
toTupleT [x] = x
toTupleT xs = appsT (tupleT (length xs)) xs

-- | Construct a tuple value given a list of expressions.
toTupleE :: [ExpQ] -> ExpQ
toTupleE [x] = x
toTupleE xs = tupE xs

-- | Construct a tuple pattern given a list of patterns.
toTupleP :: [PatQ] -> PatQ
toTupleP [x] = x
toTupleP xs = tupP xs

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)

-- | Return 'Name' contained in a 'TyVarBndr'.
bndrName :: TyVarBndr -> Name
bndrName (PlainTV  n  ) = n
bndrName (KindedTV n _) = n

-- | Generate many new names from a given base name.
newNames :: String {- ^ base name -} -> Int {- ^ count -} -> Q [Name]
newNames base n = sequence [ newName (base++show i) | i <- [1..n] ]

------------------------------------------------------------------------
-- Manually quoted names
------------------------------------------------------------------------
-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling the lens library.
-- This allows the library to be used in stage1 cross-compilers.

opticsPackageKey       :: String
#ifdef CURRENT_PACKAGE_KEY
opticsPackageKey        = CURRENT_PACKAGE_KEY
#else
opticsPackageKey        = "optics-" ++ showVersion version
#endif

mkOpticsName_tc        :: String -> String -> Name
mkOpticsName_tc         = mkNameG_tc opticsPackageKey

mkOpticsName_v         :: String -> String -> Name
mkOpticsName_v          = mkNameG_v opticsPackageKey

{-

traversalTypeName      :: Name
traversalTypeName       = mkOpticsName_tc "Optics.Internal.Traversal" "Traversal"

traversal'TypeName     :: Name
traversal'TypeName      = mkOpticsName_tc "Optics.Internal.Traversal" "Traversal'"

lensTypeName           :: Name
lensTypeName            = mkOpticsName_tc "Optics.Internal.Lens" "Lens"

lens'TypeName          :: Name
lens'TypeName           = mkOpticsName_tc "Optics.Internal.Lens" "Lens'"

isoTypeName            :: Name
isoTypeName             = mkOpticsName_tc "Optics.Internal.Iso" "Iso"

iso'TypeName           :: Name
iso'TypeName            = mkOpticsName_tc "Optics.Internal.Iso" "Iso'"

getterTypeName         :: Name
getterTypeName          = mkOpticsName_tc "Optics.Internal.Getter" "Getter"

foldTypeName           :: Name
foldTypeName            = mkOpticsName_tc "Optics.Internal.Fold" "Fold"

prismTypeName          :: Name
prismTypeName           = mkOpticsName_tc "Optics.Internal.Prism" "Prism"

prism'TypeName         :: Name
prism'TypeName          = mkOpticsName_tc "Optics.Internal.Prism" "Prism'"

reviewTypeName          :: Name
reviewTypeName           = mkOpticsName_tc "Optics.Internal.Review" "Review"

isoValName              :: Name
isoValName               = mkOpticsName_v "Optics.Internal.Iso" "iso"

prismValName            :: Name
prismValName             = mkOpticsName_v "Optics.Internal.Prism" "prism"

untoValName             :: Name
untoValName              = mkOpticsName_v "Optics.Internal.Review" "unto"

phantomValName          :: Name
phantomValName           = mkOpticsName_v "Optics.Internal.TH" "phantom2"

-}

composeValName          :: Name
composeValName           = mkNameG_v "base" "GHC.Base" "."

idValName               :: Name
idValName                = mkNameG_v "base" "GHC.Base" "id"

fmapValName             :: Name
fmapValName              = mkNameG_v "base" "GHC.Base" "fmap"

pureValName             :: Name
pureValName              = mkNameG_v "base" "GHC.Base" "pure"

apValName               :: Name
apValName                = mkNameG_v "base" "GHC.Base" "<*>"

rightDataName           :: Name
rightDataName            = mkNameG_d "base" "Data.Either" "Right"

leftDataName            :: Name
leftDataName             = mkNameG_d "base" "Data.Either" "Left"


------------------------------------------------------------------------
-- Support for generating inline pragmas
------------------------------------------------------------------------

inlinePragma :: Name -> [DecQ]
inlinePragma methodName = [pragInlD methodName Inline FunLike AllPhases]

------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------

phantom :: f a -> f b
phantom = undefined

both :: Traversal (a, a) (b, b) a b
both = traversalVL $ \f (a1, a2) -> (,) <$> f a1 <*> f a2

setOf :: (Is k A_Fold, Ord a) => Optic' k is s a -> s -> Set.Set a
setOf l = foldMapOf l Set.singleton
