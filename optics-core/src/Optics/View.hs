{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.View where

import Control.Monad.Reader.Class

import Optics.AffineFold
import Optics.AffineTraversal
import Optics.Fold
import Optics.Getter
import Optics.Iso
import Optics.IxFold
import Optics.IxTraversal
import Optics.Lens
import Optics.Prism
import Optics.PrismaticGetter

-- | View the value(s) pointed to by an optic.
--
-- The type of the result depends on the optic. You get:
--
--   * Exactly one result @a@ with 'Iso', 'Lens', 'PrismaticGetter' and
--   'Getter'.
--
--   * At most one result @Maybe a@ with 'Prism', 'AffineTraversal' and
--   'AffineFold'.
--
--   * Multiple results @[a]@ with 'Optics.Traversal.Traversal' and 'Fold'.
--
--   * Multiple results along with their indices @[(i, a)]@ with
--   'IxTraversal' and 'IxFold'.
--
class ViewableOptic k is a where
  type ViewResult k is a :: *
  view :: Optic' k is s a -> s -> ViewResult k is a

instance is ~ NoIx => ViewableOptic An_Iso is a where
  type ViewResult An_Iso is a = a
  view = view1
  {-# INLINE view #-}

instance is ~ NoIx => ViewableOptic A_Lens is a where
  type ViewResult A_Lens is a = a
  view = view1
  {-# INLINE view #-}

instance is ~ NoIx => ViewableOptic A_PrismaticGetter is a where
  type ViewResult A_PrismaticGetter is a = a
  view = view1
  {-# INLINE view #-}

instance is ~ NoIx => ViewableOptic A_Getter is a where
  type ViewResult A_Getter is a = a
  view = view1
  {-# INLINE view #-}

instance is ~ NoIx => ViewableOptic A_Prism is a where
  type ViewResult A_Prism is a = Maybe a
  view = view01
  {-# INLINE view #-}

instance is ~ NoIx => ViewableOptic An_AffineTraversal is a where
  type ViewResult An_AffineTraversal is a = Maybe a
  view = view01
  {-# INLINE view #-}

instance is ~ NoIx => ViewableOptic An_AffineFold is a where
  type ViewResult An_AffineFold is a = Maybe a
  view = view01
  {-# INLINE view #-}

instance ViewableOptic A_Traversal NoIx a where
  type ViewResult A_Traversal NoIx a = [a]
  view = toListOf
  {-# INLINE view #-}

instance (CheckIndices "view" 1 i (i ': is)
         ) => ViewableOptic A_Traversal (i ': is) a where
  type ViewResult A_Traversal (i ': is) a = [(i, a)]
  view = itoListOf
  {-# INLINE view #-}

instance ViewableOptic A_Fold NoIx a where
  type ViewResult A_Fold NoIx a = [a]
  view = toListOf
  {-# INLINE view #-}

instance (CheckIndices "view" 1 i (i ': is)
         ) => ViewableOptic A_Fold (i ': is) a where
  type ViewResult A_Fold (i ': is) a = [(i, a)]
  view = itoListOf
  {-# INLINE view #-}

-- | Generalization of 'view' from @(->) s@ to arbitrary @MonadReader s m@.
viewM
  :: (ViewableOptic k is a, MonadReader s m)
  => Optic' k is s a
  -> m (ViewResult k is a)
viewM = asks . view
