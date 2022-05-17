module MMP.Optics.Re where

import           Control.Monad
import           Optics

import           MetaMetaPost
import           MMP.Optics.Common

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

    arrow Tag_Iso Tag_Lens
    arrow Tag_Iso Tag_Prism
    arrow Tag_Iso Tag_ReversedLens
    arrow Tag_Iso Tag_ReversedPrism

    arrow Tag_ReversedLens Tag_Review
    arrow Tag_Prism Tag_Review

    arrow Tag_Lens Tag_Getter
    arrow Tag_ReversedPrism Tag_Getter

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

    -- Lens <-> ReversedLens & Prism <-> ReversedPrism
    prismReview <- bind_ $ path Tag_Prism Tag_Review
    lensGetter  <- bind_ $ path Tag_Lens Tag_Getter

    prismReversedPrism <- bind_ $ P $
        (z ^. rix Tag_ReversedPrism, L 168) .... (z ^. rix Tag_Prism)
    lensReversedLens      <- bind_ $ P $
        (z ^. rix Tag_Lens, L 168) .... (z ^. rix Tag_ReversedLens)

    it1 <- bindSnd_ $ lensGetter `IntersectionTimes` prismReversedPrism
    ip1 <- bind_    $ lensGetter `IntersectionPoint` prismReversedPrism
    ic1 <- bind_    $ Circle (L 2) ip1

    it2 <- bindSnd_ $ prismReversedPrism `IntersectionTimes` lensReversedLens
    ip2 <- bind_    $ prismReversedPrism `IntersectionPoint` lensReversedLens
    ic2 <- bind_    $ Circle (L 8) ip2

    it3 <- bindSnd_ $ prismReview `IntersectionTimes` lensReversedLens
    ip3 <- bind_    $ prismReview `IntersectionPoint` lensReversedLens
    ic3 <- bind_    $ Circle (L 2) ip3

    -- Prism <-> ReversedPrism
    drawarrowC_ red $ clippath'
        (subpath_ (L 0, it1) prismReversedPrism)
        ic1
        (bbox_ $ q ^. rix Tag_ReversedPrism)
    drawarrowC_ red $ reverse_ $ clippath'
        (subpath_ (it1, length_ prismReversedPrism) prismReversedPrism)
        (bbox_ $ q ^. rix Tag_Prism)
        ic1

    -- Lens <-> ReversedLens pieces
    drawarrowC_ red $ clippath'
        (subpath_ (it3, length_ lensReversedLens) lensReversedLens)
        ic3
        (bbox_ $ q ^. rix Tag_ReversedLens)

    drawC_ red $ clippath'
        (subpath_ (it3, it2) lensReversedLens)
        ic3
        ic2

    drawarrowC_ red $ reverse_ $ clippath'
        (subpath_ (L 0, it2) lensReversedLens)
        (bbox_ $ q ^. rix Tag_Lens)
        ic2

    -- Iso
    -- TODO: add crossings...
    drawarrowC_ red $ P $
        (z ^. rix Tag_Iso .+ Pair 5 5, L 80) ...
        (z ^. rix Tag_Iso .+ Pair 20 0, L 270) ....
        (z ^. rix Tag_Iso .+ Pair 5 (-5), L 100)

  where
    isRe Tag_Iso             = True
    isRe Tag_Lens            = True
    isRe Tag_Prism           = True
    isRe Tag_ReversedLens    = True
    isRe Tag_ReversedPrism   = True
    isRe Tag_Getter          = True
    isRe Tag_Review          = True
    isRe _                   = False
