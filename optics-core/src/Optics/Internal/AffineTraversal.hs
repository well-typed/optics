module Optics.Internal.AffineTraversal where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying affine traversal.
type AffineTraversal s t a b = Optic An_AffineTraversal NoIx s t a b

-- | Type synonym for a type-preserving affine traversal.
type AffineTraversal' s a = Optic' An_AffineTraversal NoIx s a

-- | Explicitly cast an optic to an affine traversal.
toAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k                  is s t a b
  -> Optic An_AffineTraversal is s t a b
toAffineTraversal = castOptic
{-# INLINE toAffineTraversal #-}

-- | Build an affine traversal from a matcher and an updater.
atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
atraversal match update = Optic $
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
{-# INLINE atraversal #-}

withAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k is s t a b
  -> ((s -> b -> t) -> (s -> Either t a) -> r)
  -> r
withAffineTraversal o k =
  case getOptic (toAffineTraversal o) (AffineMarket (\_ b -> b) Right) of
    AffineMarket setter match -> k setter match

----------------------------------------

-- | Type to represent the components of a affine traversal
data AffineMarket a b is s t = AffineMarket (s -> b -> t) (s -> Either t a)

instance Profunctor (AffineMarket a b) where
  dimap f g (AffineMarket sbt seta) = AffineMarket
    (\s b -> g (sbt (f s) b))
    (either (Left . g) Right . seta . f)
  {-# INLINE dimap #-}

instance Choice (AffineMarket a b) where
  left' (AffineMarket sbt seta) = AffineMarket
    (\e b -> bimapE (flip sbt b) id e)
    (\sc -> case sc of
      Left s -> bimapE Left id (seta s)
      Right c -> Left (Right c))

  right' (AffineMarket sbt seta) = AffineMarket
    (\e b -> bimapE id (flip sbt b) e)
    (\sc -> case sc of
      Left c -> Left (Left c)
      Right s -> bimapE Right id (seta s))
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Strong (AffineMarket a b) where
  first' (AffineMarket sbt seta) = AffineMarket
    (\(a, c) b -> (sbt a b, c))
    (\(a, c) -> bimapE (,c) id (seta a))

  second' (AffineMarket sbt seta) = AffineMarket
    (\(c, a) b -> (c, sbt a b))
    (\(c, a) -> bimapE (c,) id (seta a))

  {-# INLINE first' #-}
  {-# INLINE second' #-}

bimapE :: (a -> b) -> (c -> d) -> Either a c -> Either  b d
bimapE f _ (Left a)  = Left (f a)
bimapE _ g (Right b) = Right (g b)
