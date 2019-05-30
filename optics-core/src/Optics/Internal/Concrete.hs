{-# OPTIONS_HADDOCK not-home #-}

-- | Concrete representation types for certain optics.
--
-- This module is intended for internal use only, and may change without warning
-- in subsequent releases.
module Optics.Internal.Concrete
  ( Exchange(..)
  , Store(..)
  , Market(..)
  , AffineMarket(..)
  ) where

import Data.Bifunctor

import Optics.Internal.Profunctor

-- | Type to represent the components of an isomorphism.
data Exchange a b i ci s t =
  Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap ss tt (Exchange sa bt) = Exchange (sa . ss) (tt . bt)
  lmap  ss    (Exchange sa bt) = Exchange (sa . ss) bt
  rmap     tt (Exchange sa bt) = Exchange sa        (tt . bt)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

-- | Type to represent the components of a lens.
data Store a b i ci s t = Store (s -> a) (s -> b -> t)

instance Profunctor (Store a b) where
  dimap f g (Store get set) = Store (get . f) (\s -> g . set (f s))
  lmap  f   (Store get set) = Store (get . f) (\s -> set (f s))
  rmap    g (Store get set) = Store get       (\s -> g . set s)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Strong (Store a b) where
  first' (Store get set) = Store (get . fst) (\(s, c) b -> (set s b, c))
  second' (Store get set) = Store (get . snd) (\(c, s) b -> (c, set s b))
  {-# INLINE first' #-}
  {-# INLINE second' #-}

-- | Type to represent the components of a prism.
data Market a b i ci s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b i ci s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)
  {-# INLINE fmap #-}

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)
  lmap  f   (Market bt seta) = Market bt (seta . f)
  rmap    g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice (Market a b) where
  left' (Market bt seta) = Market (Left . bt) $ \sc -> case sc of
    Left s -> case seta s of
      Left t -> Left (Left t)
      Right a -> Right a
    Right c -> Left (Right c)
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a
  {-# INLINE left' #-}
  {-# INLINE right' #-}

-- | Type to represent the components of an affine traversal.
data AffineMarket a b i ci s t = AffineMarket (s -> b -> t) (s -> Either t a)

instance Profunctor (AffineMarket a b) where
  dimap f g (AffineMarket sbt seta) = AffineMarket
    (\s b -> g (sbt (f s) b))
    (either (Left . g) Right . seta . f)
  lmap f (AffineMarket sbt seta) = AffineMarket
    (\s b -> sbt (f s) b)
    (seta . f)
  rmap g (AffineMarket sbt seta) = AffineMarket
    (\s b -> g (sbt s b))
    (either (Left . g) Right . seta)
  {-# INLINE dimap #-}
  {-# INLINE lmap #-}
  {-# INLINE rmap #-}

instance Choice (AffineMarket a b) where
  left' (AffineMarket sbt seta) = AffineMarket
    (\e b -> bimap (flip sbt b) id e)
    (\sc -> case sc of
      Left s -> bimap Left id (seta s)
      Right c -> Left (Right c))
  right' (AffineMarket sbt seta) = AffineMarket
    (\e b -> bimap id (flip sbt b) e)
    (\sc -> case sc of
      Left c -> Left (Left c)
      Right s -> bimap Right id (seta s))
  {-# INLINE left' #-}
  {-# INLINE right' #-}

instance Strong (AffineMarket a b) where
  first' (AffineMarket sbt seta) = AffineMarket
    (\(a, c) b -> (sbt a b, c))
    (\(a, c) -> bimap (,c) id (seta a))
  second' (AffineMarket sbt seta) = AffineMarket
    (\(c, a) b -> (c, sbt a b))
    (\(c, a) -> bimap (c,) id (seta a))
  {-# INLINE first' #-}
  {-# INLINE second' #-}

instance Visiting (AffineMarket a b)
