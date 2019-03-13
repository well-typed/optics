{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main (main) where

import           Control.Monad.State
import           Generics.SOP
import qualified GHC.Generics        as GHC
import           System.Environment  (getArgs)

import           Optics
import           Optics.Operators

import           MetaMetaPost

-------------------------------------------------------------------------------
-- representable with Rep f = OpticKind
-------------------------------------------------------------------------------

data OK
    =  Tag_Equality
    |  Tag_Iso
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
    |  Tag_IxTraversal
    |  Tag_IxSetter
    |  Tag_IxFold
    |  Tag_IxAffineTraversal
    |  Tag_IxAffineFold
  deriving (Eq, Ord, Read, Show, Enum, Bounded, GHC.Generic)

instance Generic OK

-- | There should be enough @a@
data PerOK a = PerOK a a a a a a a a a a a a a a a a a a
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
okName Tag_Equality          = "Equality"
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
okName Tag_AffineFold        = "AffineFold"
okName Tag_Fold              = "Fold"
okName Tag_IxFold            = "IxFold"
okName Tag_LensyReview       = "LensyReview"
okName Tag_Review            = "Review"
okName Tag_IxAffineFold      = "IxAffineFold"
okName Tag_IxAffineTraversal = "IxAffineTraversal"

-- | We need an offset to avoid empty space
-- For some reason metapost doesn't cut it itself :(
positions :: PerOK (Expr s 'Product)
positions = tabulate $ \case
    Tag_Equality          -> pair 2 0
    Tag_Iso               -> pair 2 1
    Tag_Lens              -> pair 1 2
    Tag_Prism             -> pair 3 2
    Tag_AffineTraversal   -> pair 2 3
    Tag_IxAffineTraversal -> pair 1 4
    Tag_Traversal         -> pair 3 4
    Tag_IxTraversal       -> pair 2 5
    Tag_Setter            -> pair 4 5
    Tag_IxSetter          -> pair 3 6
    Tag_PrismaticGetter   -> pair 0 2
    Tag_Getter            -> pair 1 3
    Tag_AffineFold        -> pair 2 4
    Tag_IxAffineFold      -> pair 1 5
    Tag_Fold              -> pair 3 5
    Tag_IxFold            -> pair 2 6
    Tag_LensyReview       -> pair 4 2
    Tag_Review            -> pair 3 3
  where
    pair x y = Pair (L x .* dimX) (L y .* dimY)

-------------------------------------------------------------------------------
-- Diagrams: Hierarchy
-------------------------------------------------------------------------------

hierarchy :: Stmts s ()
hierarchy = do
    clippath <- vardef3 "clippath" SPath SPicture SPicture $ \p a b -> do
        t1 <- bindSnd_ $ bbox_ a `IntersectionTimes` p
        t2 <- bindSnd_ $ bbox_ b `IntersectionTimes` reverse_ p
        return $ subpath_ (t1, length_ p .- t2) p

    z <- traverse bind_ positions
    q <- itraverse (\k -> bind_ . TheLabel ("\\mathit{" ++ okName k ++ "}")) z

    ifor_ q $ \k pic -> unless (isIndexed k) $ draw_ pic

    -- arrows
    let arrow a b = drawarrow_ $ clippath
            (P $ (z ^. rix a) ... pathEnd (z ^. rix b))
            (q ^. rix a)
            (q ^. rix b)

    arrow Tag_Equality Tag_Iso
    arrow Tag_Iso Tag_Lens
    arrow Tag_Iso Tag_Prism
    arrow Tag_Iso Tag_LensyReview
    arrow Tag_Iso Tag_PrismaticGetter

    arrow Tag_LensyReview Tag_Review
    arrow Tag_Prism Tag_Review
    arrow Tag_Prism Tag_AffineTraversal

    arrow Tag_Lens Tag_AffineTraversal
    arrow Tag_AffineTraversal Tag_Traversal
    arrow Tag_Traversal Tag_Setter

    arrow Tag_PrismaticGetter Tag_Getter
    arrow Tag_Getter Tag_AffineFold
    arrow Tag_AffineFold Tag_Fold

    arrow Tag_Lens Tag_Getter
    arrow Tag_AffineTraversal Tag_AffineFold
    arrow Tag_Traversal Tag_Fold
  where
    isIndexed Tag_IxTraversal = True
    isIndexed Tag_IxFold      = True
    isIndexed Tag_IxSetter    = True
    isIndexed _               = False

-------------------------------------------------------------------------------
-- Optics.Re
-------------------------------------------------------------------------------

reOptics :: Stmts s ()
reOptics = do
    clippath <- vardef3 "clippath" SPath SPicture SPicture $ \p a b -> do
        t1 <- bindSnd_ $ bbox_ a `IntersectionTimes` p
        t2 <- bindSnd_ $ bbox_ b `IntersectionTimes` reverse_ p
        return $ subpath_ (t1, length_ p .- t2) p

    clippath' <- vardef3 "clippath" SPath SPath SPath  $ \p a b -> do
        t1 <- bindSnd_ $ bbox_ a `IntersectionTimes` p
        t2 <- bindSnd_ $ bbox_ b `IntersectionTimes` reverse_ p
        return $ subpath_ (t1, length_ p .- t2) p

    clippathend <- vardef2 "clippathend" SPath SPicture $ \p a -> do
        t2 <- bindSnd_ $ bbox_ a `IntersectionTimes` reverse_ p
        return $ subpath_ (L 0, length_ p .- t2) p

    z <- traverse bind_ positions
    q <- itraverse (\k -> bind_ . TheLabel ("\\mathit{" ++ okName k ++ "}")) z

    ifor_ q  $ \k pic -> when (isRe k) $ draw_ pic

    -- arrows
    let path a b = P $ z ^. rix a .... z ^. rix b
    let arrow a b = drawarrow_ $ clippath (path a b) (q ^. rix a) (q ^. rix b)

    arrow Tag_Equality Tag_Iso
    arrow Tag_Iso Tag_Lens
    arrow Tag_Iso Tag_Prism
    arrow Tag_Iso Tag_LensyReview
    arrow Tag_Iso Tag_PrismaticGetter

    arrow Tag_LensyReview Tag_Review
    arrow Tag_Prism Tag_Review

    arrow Tag_Lens Tag_Getter
    arrow Tag_PrismaticGetter Tag_Getter

    -- Getter <-> Review
    red <- bind_ $ RGB (L 0.5) (L 0) (L 0)

    getterReview <- bind_ $ P $ (z ^. rix Tag_Getter, L 160) .... (z ^. rix Tag_Review)
    getterReview1 <- bind_ $ subpath_ (L 0.5 .* length_ getterReview, length_ getterReview) getterReview
    getterReview2 <- bind_ $ reverse_ $ subpath_ (L 0, L 0.5 .* length_ getterReview) getterReview

    drawarrowC_ red $ clippathend
        getterReview1
        (q ^. rix Tag_Review)
    drawarrowC_ red $ clippathend
        getterReview2
        (q ^. rix Tag_Getter)

    -- Lens <-> LensyReview & Prism <-> PrismaticGetter
    prismReview <- bind_ $ path Tag_Prism Tag_Review
    lensGetter  <- bind_ $ path Tag_Lens Tag_Getter

    prismPrismaticGetter <- bind_ $ P $
        (z ^. rix Tag_PrismaticGetter, L 168) .... (z ^. rix Tag_Prism)
    lensLensyReview      <- bind_ $ P $
        (z ^. rix Tag_Lens, L 168) .... (z ^. rix Tag_LensyReview)

    it1 <- bindSnd_ $ lensGetter `IntersectionTimes` prismPrismaticGetter
    ip1 <- bind_    $ lensGetter `IntersectionPoint` prismPrismaticGetter
    ic1 <- bind_    $ Circle (L 2) ip1

    it2 <- bindSnd_ $ prismPrismaticGetter `IntersectionTimes` lensLensyReview
    ip2 <- bind_    $ prismPrismaticGetter `IntersectionPoint` lensLensyReview
    ic2 <- bind_    $ Circle (L 8) ip2

    it3 <- bindSnd_ $ prismReview `IntersectionTimes` lensLensyReview
    ip3 <- bind_    $ prismReview `IntersectionPoint` lensLensyReview
    ic3 <- bind_    $ Circle (L 2) ip3

    -- Prism <-> PrismaticGetter
    drawarrowC_ red $ clippath'
        (subpath_ (L 0, it1) prismPrismaticGetter)
        ic1
        (bbox_ $ q ^. rix Tag_PrismaticGetter)
    drawarrowC_ red $ reverse_ $ clippath'
        (subpath_ (it1, length_ prismPrismaticGetter) prismPrismaticGetter)
        (bbox_ $ q ^. rix Tag_Prism)
        ic1

    -- Lens <-> LensyReview pieces
    drawarrowC_ red $ clippath'
        (subpath_ (it3, length_ lensLensyReview) lensLensyReview)
        ic3
        (bbox_ $ q ^. rix Tag_LensyReview)

    drawC_ red $ clippath'
        (subpath_ (it3, it2) lensLensyReview)
        ic3
        ic2

    drawarrowC_ red $ reverse_ $ clippath'
        (subpath_ (L 0, it2) lensLensyReview)
        (bbox_ $ q ^. rix Tag_Lens)
        ic2

    -- Iso
    -- TODO: add crossings...
    drawarrowC_ red $ P $
        (z ^. rix Tag_Iso .+ Pair 5 5, L 80) ...
        (z ^. rix Tag_Iso .+ Pair 20 0, L 270) ....
        (z ^. rix Tag_Iso .+ Pair 5 (-5), L 100)

    -- Equality
    drawarrowC_ red $ P $
        (z ^. rix Tag_Equality .+ Pair 15 5, L 80) ...
        (z ^. rix Tag_Equality .+ Pair 30 0, L 270) ....
        (z ^. rix Tag_Equality .+ Pair 15 (-5), L 100)

  where
    isRe Tag_Equality        = True
    isRe Tag_Iso             = True
    isRe Tag_Lens            = True
    isRe Tag_Prism           = True
    isRe Tag_LensyReview     = True
    isRe Tag_PrismaticGetter = True
    isRe Tag_Getter          = True
    isRe Tag_Review          = True
    isRe _                   = False

-------------------------------------------------------------------------------
-- Optics.Indexed
-------------------------------------------------------------------------------

indexedOptics :: forall s. Stmts s ()
indexedOptics = do
    clippath <- vardef3 "clippath" SPath SPicture SPicture $ \p a b -> do
        t1 <- bindSnd_ $ bbox_ a `IntersectionTimes` p
        t2 <- bindSnd_ $ bbox_ b `IntersectionTimes` reverse_ p
        return $ subpath_ (t1, length_ p .- t2) p

    clippath' <- vardef3 "clippath" SPath SPath SPath  $ \p a b -> do
        t1 <- bindSnd_ $ bbox_ a `IntersectionTimes` p
        t2 <- bindSnd_ $ bbox_ b `IntersectionTimes` reverse_ p
        return $ subpath_ (t1, length_ p .- t2) p

    z <- traverse bind_ positions
    q <- itraverse (\k -> bind_ . TheLabel ("\\mathit{" ++ okName k ++ "}")) z

    blue <- bind_ $ RGB (L 0) (L 0) (L 0.6)

    ifor_ q  $ \k pic -> when (isIxRelated k) $
        if isIx k
        then drawC_ blue pic
        else draw_ pic

    -- arrows
    let path a b = P $ z ^. rix a .... z ^. rix b
    let arrow a b = drawarrow_ $ clippath (path a b) (q ^. rix a) (q ^. rix b)
    let arrowC c a b = drawarrowC_ c $ clippath (path a b) (q ^. rix a) (q ^. rix b)

    let arrowCrossingC :: Expr s 'Color -> OK -> OK -> OK -> OK -> Stmts s ()
        arrowCrossingC c a b u v = do
          let ab = path a b
          let uv = path u v

          -- crossing
          ct <- bindSnd_ $ uv `IntersectionTimes` ab
          cx <- bind_    $ uv `IntersectionPoint` ab -- TODO: use point of
          cc <- bind_    $ Circle (L 2) cx

          drawC_ c $ clippath'
              (subpath_ (L 0, ct) ab)
              (bbox_ $ q ^. rix a)
              cc

          drawarrowC_ c $ clippath'
              (subpath_ (ct, length_ ab) ab)
              cc
              (bbox_ $ q ^. rix b)

    black <- bind_ $ RGB (L 0) (L 0) (L 0)

    arrow Tag_Lens Tag_AffineTraversal
    arrow Tag_AffineTraversal Tag_Traversal
    arrow Tag_Traversal Tag_Setter

    arrow Tag_Getter Tag_AffineFold
    arrowC black Tag_AffineFold Tag_Fold

    arrow Tag_Lens Tag_Getter
    arrow Tag_AffineTraversal Tag_AffineFold
    arrow Tag_Traversal Tag_Fold

    -- arrows to indexed optics

    orange <- bind_ $ RGB (L 1) (L 0.5) (L 0.2)

    arrowCrossingC orange Tag_Traversal       Tag_IxTraversal Tag_AffineFold Tag_Fold
    arrowC         orange Tag_Setter          Tag_IxSetter
    arrowC         orange Tag_Fold            Tag_IxFold
    arrowC         orange Tag_AffineFold      Tag_IxAffineFold
    arrowCrossingC orange Tag_AffineTraversal Tag_IxAffineTraversal Tag_Getter Tag_AffineFold

    -- Arrows between indexed optics

    arrowC         blue Tag_IxAffineFold      Tag_IxFold
    arrowCrossingC blue Tag_IxAffineTraversal Tag_IxTraversal   Tag_AffineFold Tag_IxAffineFold
    arrowC         blue Tag_IxTraversal       Tag_IxFold
    arrowC         blue Tag_IxAffineTraversal Tag_IxAffineFold
    arrowCrossingC blue Tag_IxTraversal       Tag_IxSetter      Tag_Fold Tag_IxFold

    return ()
  where
    isIxRelated Tag_Lens            = True
    isIxRelated Tag_AffineTraversal = True
    isIxRelated Tag_Traversal       = True
    isIxRelated Tag_Setter          = True

    isIxRelated Tag_Getter          = True
    isIxRelated Tag_AffineFold      = True
    isIxRelated Tag_Fold            = True

    isIxRelated Tag_IxTraversal       = True
    isIxRelated Tag_IxSetter          = True
    isIxRelated Tag_IxAffineFold      = True
    isIxRelated Tag_IxAffineTraversal = True

    isIxRelated Tag_IxFold          = True

    isIxRelated _                   = False

    isIx Tag_IxTraversal       = True
    isIx Tag_IxSetter          = True
    isIx Tag_IxFold            = True
    isIx Tag_IxAffineTraversal = True
    isIx Tag_IxAffineFold      = True

    isIx _ = False


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("hierarchy" : _)     -> printDiagram hierarchy
        ("reoptics" : _)      -> printDiagram reOptics
        ("indexedoptics" : _) -> printDiagram indexedOptics
        _                     -> printDiagram hierarchy
