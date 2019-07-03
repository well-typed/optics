{-# LANGUAGE DataKinds #-}
module MMP.Optics.Hierarchy where

import           Control.Monad.State
import qualified Data.Graph as G
import           Optics

import MMP.Optics.Common
import MetaMetaPost

hierarchy :: Stmts s ()
hierarchy = hierarchyColoured (\_ -> black) (\ _ _ -> black)

hierarchyColoured :: (OK -> Expr s 'Color)  -- ^ Colour for optic kind label
                  -> (OK -> OK -> Expr s 'Color)  -- ^ Colour for arrow from first optic kind to second
                  -> Stmts s ()
hierarchyColoured text_colour arrow_colour = do
    clippath <- vardef3 "clippath" SPath SPicture SPicture $ \p a b -> do
        t1 <- bindSnd_ $ bbox_ a `IntersectionTimes` p
        t2 <- bindSnd_ $ bbox_ b `IntersectionTimes` reverse_ p
        return $ subpath_ (t1, length_ p .- t2) p

    z <- traverse bind_ positions
    q <- itraverse (\k -> bind_ . TheLabel ("\\mathit{" ++ okName k ++ "}")) z

    ifor_ q $ \k pic -> unless (isIndexed k) $ drawC_ (text_colour k) pic

    -- arrows
    let arrow a b = drawarrowC_ (arrow_colour a b) $ clippath
            (P $ (z ^. rix a) ... pathEnd (z ^. rix b))
            (q ^. rix a)
            (q ^. rix b)

    mapM_ (uncurry arrow) edges

edges :: [(OK, OK)]
edges = [ (Tag_Iso, Tag_Lens)
        , (Tag_Iso, Tag_Prism)
        , (Tag_Iso, Tag_ReversedLens)
        , (Tag_Iso, Tag_ReversedPrism)

        , (Tag_ReversedLens, Tag_Review)
        , (Tag_Prism, Tag_Review)
        , (Tag_Prism, Tag_AffineTraversal)

        , (Tag_Lens, Tag_AffineTraversal)
        , (Tag_AffineTraversal, Tag_Traversal)
        , (Tag_Traversal, Tag_Setter)

        , (Tag_ReversedPrism, Tag_Getter)
        , (Tag_Getter, Tag_AffineFold)
        , (Tag_AffineFold, Tag_Fold)

        , (Tag_Lens, Tag_Getter)
        , (Tag_AffineTraversal, Tag_AffineFold)
        , (Tag_Traversal, Tag_Fold)
        ]

isIndexed :: OK -> Bool
isIndexed Tag_IxAffineFold      = True
isIndexed Tag_IxAffineTraversal = True
isIndexed Tag_IxFold            = True
isIndexed Tag_IxSetter          = True
isIndexed Tag_IxTraversal       = True
isIndexed Tag_IxLens            = True
isIndexed Tag_IxGetter          = True
isIndexed _                     = False

graph :: G.Graph
graph = G.buildG (fromEnum (minBound :: OK), fromEnum (maxBound :: OK))
                 (map (\ (x, y) -> (fromEnum x, fromEnum y)) edges)

hierarchyFocus :: OK -> Stmts s ()
hierarchyFocus focus = hierarchyColoured (text_colour . fromEnum)
                                         (\ a b -> arrow_colour (fromEnum a) (fromEnum b))
  where
    f = fromEnum focus

    text_colour k
      | k == f           = red
      | G.path graph f k = black
      | G.path graph k f = black
      | otherwise        = grey

    arrow_colour j k
      | G.path graph f j, G.path graph f k = black
      | G.path graph j f, G.path graph k f = black
      | otherwise                          = grey

red, grey, black :: Expr s 'Color
red        = RGB (L 0.75) (L 0)    (L 0)
grey       = RGB (L 0.75) (L 0.75) (L 0.75)
black      = RGB (L 0)    (L 0)    (L 0)
