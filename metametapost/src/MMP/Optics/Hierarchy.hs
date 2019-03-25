module MMP.Optics.Hierarchy where

import           Control.Monad.State
import           Optics
import           Optics.Operators

import MMP.Optics.Common
import MetaMetaPost

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
    isIndexed Tag_IxAffineFold      = True
    isIndexed Tag_IxAffineTraversal = True
    isIndexed Tag_IxFold            = True
    isIndexed Tag_IxSetter          = True
    isIndexed Tag_IxTraversal       = True
    isIndexed _                     = False
