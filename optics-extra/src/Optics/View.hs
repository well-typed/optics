-- | EXPERIMENTAL
module Optics.View where

import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.Writer

import Optics.Core

-- | Generalized view (even more powerful than @view@ from the lens library).
--
-- View the value(s) pointed to by an optic.
--
-- The type of the result depends on the optic. You get:
--
--   * Exactly one result @a@ with 'Iso', 'Lens', 'ReversedPrism' and
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
class ViewableOptic k r where
  type ViewResult k r :: *
  gview
    :: MonadReader s m
    => Optic' k is s r
    -> m (ViewResult k r)
  gviews
    :: MonadReader s m
    => Optic' k is s a
    -> (a -> r)
    -> m (ViewResult k r)

instance ViewableOptic An_Iso r where
  type ViewResult An_Iso r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_Lens r where
  type ViewResult A_Lens r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_ReversedPrism r where
  type ViewResult A_ReversedPrism r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_Getter r where
  type ViewResult A_Getter r = r
  gview    = asks . view
  gviews o = asks . views o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic A_Prism r where
  type ViewResult A_Prism r = Maybe r
  gview    = asks . preview
  gviews o = asks . previews o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic An_AffineTraversal r where
  type ViewResult An_AffineTraversal r = Maybe r
  gview    = asks . preview
  gviews o = asks . previews o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance ViewableOptic An_AffineFold r where
  type ViewResult An_AffineFold r = Maybe r
  gview    = asks . preview
  gviews o = asks . previews o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance Monoid r => ViewableOptic A_Traversal r where
  type ViewResult A_Traversal r = r
  gview    = asks . foldOf
  gviews o = asks . foldMapOf o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

instance Monoid r => ViewableOptic A_Fold r where
  type ViewResult A_Fold r = r
  gview    = asks . foldOf
  gviews o = asks . foldMapOf o
  {-# INLINE gview #-}
  {-# INLINE gviews #-}

-- | Use the target of a 'Lens', 'Iso', or 'Getter' in the current state, or use
-- a summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- >>> evalState (use _1) ('a','b')
-- 'a'
--
-- >>> evalState (use _2) ("hello","world")
-- "world"
use
  :: (ViewableOptic k a, MonadState s m)
  => Optic' k is s a
  -> m (ViewResult k a)
use o = gets (gview o)
{-# INLINE use #-}

-- | Use the target of a 'Lens', 'Iso' or 'Getter' in the current state, or use
-- a summary of a 'Fold' or 'Traversal' that points to a monoidal value.
--
-- >>> evalState (uses _1 length) ("hello","world")
-- 5
uses
  :: (ViewableOptic k r, MonadState s m)
  => Optic' k is s a
  -> (a -> r)
  -> m (ViewResult k r)
uses o f = gets (gviews o f)
{-# INLINE uses #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'Getter'. If given a 'Fold' or a 'Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
listening
  :: (ViewableOptic k r, MonadWriter s m)
  => Optic' k is s r
  -> m a
  -> m (a, ViewResult k r)
listening o m = do
  (a, w) <- listen m
  return (a, gview o w)
{-# INLINE listening #-}

-- | This is a generalized form of 'listen' that only extracts the portion of
-- the log that is focused on by a 'Getter'. If given a 'Fold' or a 'Traversal'
-- then a monoidal summary of the parts of the log that are visited will be
-- returned.
listenings
  :: (ViewableOptic k r, MonadWriter s m)
  => Optic' k is s a
  -> (a -> r)
  -> m b
  -> m (b, ViewResult k r)
listenings o f m = do
  (a, w) <- listen m
  return (a, gviews o f w)
{-# INLINE listenings #-}
