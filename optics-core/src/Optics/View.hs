module Optics.View where

import Control.Monad.Reader.Class

import Optics.Internal.AffineFold
import Optics.Internal.Fold
import Optics.Internal.Getter
import Optics.Internal.Optic

class ViewableOptic k r where
  type ViewResult k r :: *
  -- | Apply an optic to view its result. Isos, Lenses, PrismaticGetters and
  -- Getters return exactly one, Prisms, AffineTraversals and AffineFolds return
  -- at most one, whereas Folds and Traversals return a sum (with respect to a
  -- Monoid instance) of all their results.
  view :: Optic' k is s r -> s -> ViewResult k r

instance ViewableOptic An_Iso r where
  type ViewResult An_Iso r = r
  view = view1
  {-# INLINE view #-}

instance ViewableOptic A_Lens r where
  type ViewResult A_Lens r = r
  view = view1
  {-# INLINE view #-}

instance ViewableOptic A_PrismaticGetter r where
  type ViewResult A_PrismaticGetter r = r
  view = view1
  {-# INLINE view #-}

instance ViewableOptic A_Getter r where
  type ViewResult A_Getter r = r
  view = view1
  {-# INLINE view #-}

instance ViewableOptic A_Prism r where
  type ViewResult A_Prism r = Maybe r
  view = view01
  {-# INLINE view #-}

instance ViewableOptic An_AffineTraversal r where
  type ViewResult An_AffineTraversal r = Maybe r
  view = view01
  {-# INLINE view #-}

instance ViewableOptic An_AffineFold r where
  type ViewResult An_AffineFold r = Maybe r
  view = view01
  {-# INLINE view #-}

instance Monoid r => ViewableOptic A_Traversal r where
  type ViewResult A_Traversal r = r
  view = viewN
  {-# INLINE view #-}

instance Monoid r => ViewableOptic A_Fold r where
  type ViewResult A_Fold r = r
  view = viewN
  {-# INLINE view #-}

-- | Generalization of 'view' from @(->) s@ to arbitrary @MonadReader s m@.
viewM
  :: (ViewableOptic k r, MonadReader s m)
  => Optic' k is s r
  -> m (ViewResult k r)
viewM = asks . view
