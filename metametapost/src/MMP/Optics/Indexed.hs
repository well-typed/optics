{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MMP.Optics.Indexed where

import           Control.Monad.State
import           Optics
import           Optics.Operators

import           MetaMetaPost
import           MMP.Optics.Common

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

    arrowC         orange Tag_Lens            Tag_IxLens
    arrowC         orange Tag_Getter          Tag_IxGetter

    -- Arrows between indexed optics

    arrowC         blue Tag_IxAffineFold      Tag_IxFold
    arrowCrossingC blue Tag_IxAffineTraversal Tag_IxTraversal   Tag_AffineFold Tag_IxAffineFold
    arrowC         blue Tag_IxTraversal       Tag_IxFold
    arrowC         blue Tag_IxAffineTraversal Tag_IxAffineFold
    arrowCrossingC blue Tag_IxTraversal       Tag_IxSetter      Tag_Fold Tag_IxFold

    arrowCrossingC blue Tag_IxLens            Tag_IxAffineTraversal Tag_Getter Tag_IxGetter
    arrowC         blue Tag_IxLens            Tag_IxGetter
    arrowC         blue Tag_IxGetter          Tag_IxAffineFold

    return ()
  where
    isIxRelated Tag_Lens              = True
    isIxRelated Tag_AffineTraversal   = True
    isIxRelated Tag_Traversal         = True
    isIxRelated Tag_Setter            = True

    isIxRelated Tag_Getter            = True
    isIxRelated Tag_AffineFold        = True
    isIxRelated Tag_Fold              = True

    isIxRelated Tag_IxTraversal       = True
    isIxRelated Tag_IxSetter          = True
    isIxRelated Tag_IxAffineFold      = True
    isIxRelated Tag_IxAffineTraversal = True

    isIxRelated Tag_IxFold            = True

    isIxRelated Tag_IxLens            = True
    isIxRelated Tag_IxGetter          = True

    isIxRelated _                     = False

    isIx Tag_IxTraversal       = True
    isIx Tag_IxSetter          = True
    isIx Tag_IxFold            = True
    isIx Tag_IxAffineTraversal = True
    isIx Tag_IxAffineFold      = True
    isIx Tag_IxLens            = True
    isIx Tag_IxGetter          = True

    isIx _                     = False


