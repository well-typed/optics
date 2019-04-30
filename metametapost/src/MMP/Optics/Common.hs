{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MMP.Optics.Common where

import           Generics.SOP
import qualified GHC.Generics as GHC
import           Optics

import           MetaMetaPost

-------------------------------------------------------------------------------
-- representable with Rep f = OpticKind
-------------------------------------------------------------------------------

data OK
    =  Tag_Iso
    |  Tag_Lens
    |  Tag_Prism
    |  Tag_AffineTraversal
    |  Tag_Traversal
    |  Tag_Setter
    |  Tag_PrismaticGetter
    |  Tag_Getter
    |  Tag_AffineFold
    |  Tag_Fold
    |  Tag_LensyReview
    |  Tag_Review

    -- indexed
    |  Tag_IxGetter
    |  Tag_IxLens
    |  Tag_IxTraversal
    |  Tag_IxSetter
    |  Tag_IxFold
    |  Tag_IxAffineTraversal
    |  Tag_IxAffineFold
  deriving (Eq, Ord, Read, Show, Enum, Bounded, GHC.Generic)

instance Generic OK

-- | There should be enough @a@
data PerOK a = PerOK a a a a a a a a a a a a a a a a a a a
  deriving (Functor, Foldable, Traversable, GHC.Generic)

instance Generic (PerOK a)
instance Representable OK PerOK

instance FunctorWithIndex OK PerOK
instance FoldableWithIndex OK PerOK
instance TraversableWithIndex OK PerOK where itraverse = gitraverse

-------------------------------------------------------------------------------
-- Diagrams
-------------------------------------------------------------------------------

dimX :: Expr s 'Numeric
dimX = L (-90)

dimY :: Expr s 'Numeric
dimY = L 50

okName :: OK -> String
okName Tag_Iso               = "Iso"
okName Tag_Lens              = "Lens"
okName Tag_Prism             = "Prism"
okName Tag_AffineTraversal   = "AffineTraversal"
okName Tag_Traversal         = "Traversal"
okName Tag_IxTraversal       = "IxTraversal"
okName Tag_Setter            = "Setter"
okName Tag_IxSetter          = "IxSetter"
okName Tag_PrismaticGetter   = "PrismaticGetter"
okName Tag_Getter            = "Getter"
okName Tag_IxGetter          = "IxGetter"
okName Tag_AffineFold        = "AffineFold"
okName Tag_Fold              = "Fold"
okName Tag_IxFold            = "IxFold"
okName Tag_LensyReview       = "LensyReview"
okName Tag_Review            = "Review"
okName Tag_IxLens            = "IxLens"
okName Tag_IxAffineFold      = "IxAffineFold"
okName Tag_IxAffineTraversal = "IxAffineTraversal"

-- | We need an offset to avoid empty space
-- For some reason metapost doesn't cut it itself :(
positions :: PerOK (Expr s 'Product)
positions = tabulate $ \case
    Tag_Iso               -> pair 2 1
    Tag_Lens              -> pair 1 2
    Tag_IxLens            -> pair 0 3
    Tag_Prism             -> pair 3 2
    Tag_AffineTraversal   -> pair 2 3
    Tag_IxAffineTraversal -> pair 1 4
    Tag_Traversal         -> pair 3 4
    Tag_IxTraversal       -> pair 2 5
    Tag_Setter            -> pair 4 5
    Tag_IxSetter          -> pair 3 6
    Tag_PrismaticGetter   -> pair 0 2
    Tag_Getter            -> pair 1 3
    Tag_IxGetter          -> pair 0 4
    Tag_AffineFold        -> pair 2 4
    Tag_IxAffineFold      -> pair 1 5
    Tag_Fold              -> pair 3 5
    Tag_IxFold            -> pair 2 6
    Tag_LensyReview       -> pair 4 2
    Tag_Review            -> pair 3 3
  where
    pair x y = Pair (L x .* dimX) (L y .* dimY)
