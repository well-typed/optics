module Optics.View where

import Control.Monad.Reader.Class

import Optics.Core

-- | Generalized view (even more powerful than @view@ from the lens library).
--
-- View the value(s) pointed to by an optic.
--
-- The type of the result depends on the optic. You get:
--
--   * Exactly one result @a@ with 'Iso', 'Lens', 'PrismaticGetter' and
--   'Getter'.
--
--   * At most one result @Maybe a@ with 'Prism', 'AffineTraversal' and
--   'AffineFold'.
--
--   * Monoidal summary of all results @Monoid a => a@ with 'Traversal'
--   and 'Fold'.
--
-- When in doubt, use specific, flavour restricted versions. This function is
-- mostly useful for things such as 'Passthrough'.
--
class ViewableOptic k a where
  type ViewResult k a :: *
  gview :: MonadReader s m => Optic' k is s a -> m (ViewResult k a)

instance ViewableOptic An_Iso a where
  type ViewResult An_Iso a = a
  gview = asks . view
  {-# INLINE gview #-}

instance ViewableOptic A_Lens a where
  type ViewResult A_Lens a = a
  gview = asks . view
  {-# INLINE gview #-}

instance ViewableOptic A_PrismaticGetter a where
  type ViewResult A_PrismaticGetter a = a
  gview = asks . view
  {-# INLINE gview #-}

instance ViewableOptic A_Getter a where
  type ViewResult A_Getter a = a
  gview = asks . view
  {-# INLINE gview #-}

instance ViewableOptic A_Prism a where
  type ViewResult A_Prism a = Maybe a
  gview = asks . preview
  {-# INLINE gview #-}

instance ViewableOptic An_AffineTraversal a where
  type ViewResult An_AffineTraversal a = Maybe a
  gview = asks . preview
  {-# INLINE gview #-}

instance ViewableOptic An_AffineFold a where
  type ViewResult An_AffineFold a = Maybe a
  gview = asks . preview
  {-# INLINE gview #-}

instance Monoid a => ViewableOptic A_Traversal a where
  type ViewResult A_Traversal a = a
  gview = asks . foldOf
  {-# INLINE gview #-}

instance Monoid a => ViewableOptic A_Fold a where
  type ViewResult A_Fold a = a
  gview = asks . foldOf
  {-# INLINE gview #-}

gviews
  :: (ViewableOptic k a, MonadReader s m)
  => Optic' k is s a
  -> (ViewResult k a -> r)
  -> m r
gviews o f = f <$> gview o
{-# INLINE gviews #-}
