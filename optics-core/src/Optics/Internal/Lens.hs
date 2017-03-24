{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Optics.Internal.Lens where

import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Tag for a lens.
data A_Lens

-- | Constraints corresponding to a lens.
type instance Constraints A_Lens p = Strong p

-- | Type synonym for a type-modifying lens.
type Lens s t a b = Optic A_Lens s t a b

-- | Type synonym for a type-preserving lens.
type Lens' s a = Optic' A_Lens s a

-- | Explicitly cast an optic to a lens.
toLens :: Is k A_Lens => Optic k s t a b -> Lens s t a b
toLens = sub
{-# INLINE toLens #-}

newtype UpStar f a b =
  UpStar { runUpStar :: a -> f b }

instance Functor f => Profunctor (UpStar f) where
  dimap ca bd = UpStar . dimap ca (fmap bd) . runUpStar

instance Functor f => Strong (UpStar f) where
  first'  = UpStar . dimap id (\ (fb, c) -> fmap (\ b -> (b, c)) fb) . first'  . runUpStar
  second' = UpStar . dimap id (\ (c, fb) -> fmap (\ b -> (c, b)) fb) . second' . runUpStar

instance Applicative f => Choice (UpStar f) where
  left'  = UpStar . dimap id (either (fmap Left) (pure . Right)) . left'  . runUpStar
  right' = UpStar . dimap id (either (pure . Left) (fmap Right)) . right' . runUpStar

instance Applicative f => Wandering (UpStar f) where
  wander = UpStar . dimap id sequenceA . wander . runUpStar

-- | Turn a lens into its van Laarhoven representation.
unLens :: Lens s t a b -> (forall f . Functor f => (a -> f b) -> (s -> f t))
unLens o = runUpStar . getOptic o . UpStar
{-# INLINE unLens #-}

-- | Internal representation use to convert between lens representations.
--
-- In particular,
--
-- s -> Store a b t
--
-- is isomorphic to
--
-- Lens s t a b
--
data Store a b t = Store a (b -> t)

instance Functor (Store a b) where
  fmap st (Store a bs) = Store a (st . bs)
  {-# INLINE fmap #-}

idStore :: a -> Store a b b
idStore a = Store a id
{-# INLINE idStore #-}

projStore :: Store a b t -> (b -> t, a)
projStore (Store a bt) = (bt, a)
{-# INLINE projStore #-}

-- | Create a lens from its van Laarhoven representation.
vlLens :: (forall f . Functor f => (a -> f b) -> (s -> f t)) -> Lens s t a b
vlLens o =
  mkLens (dimap (projStore . o idStore) (uncurry ($)) . second')
{-# INLINE vlLens #-}

newtype Shop a b t =
  Shop { runShop :: forall f . Functor f => (a -> f b) -> f t }

idShop :: a -> Shop a b b
idShop a = Shop ($ a)
{-# INLINE idShop #-}

instance Functor (Shop a b) where
  fmap st (Shop afbfs) = Shop (fmap st . afbfs)
  {-# INLINE fmap #-}

{-
newtype TraversableBazaar b t a =
  TraversableBazaar { runTraversableBazaar :: Bazaar a b t }

instance Functor (TraversableBazaar b t) where
  fmap = fmapDefault

instance Foldable (TraversableBazaar b t) where
  foldMap = foldMapDefault

instance Traversable (TraversableBazaar b t) where
  traverse afc (TraversableBazaar (Bazaar afbft)) =
    undefined
-}

-- wander :: (Wandering p, Traversable f) => p a b -> p (f a) (f b)

-- | Create a lens.
{-
vlLens' :: (forall f . Functor f => (a -> f b) -> (s -> f t)) -> Traversal s t a b
vlLens' o =
  mkTraversal (dimap (TraversableBazaar . o idBazaar) (runIdentity . ($ Identity) . runBazaar . runTraversableBazaar) . wander)
-}


-- | Create a lens from its profunctor representation.
mkLens :: Optic_ A_Lens s t a b -> Lens s t a b
mkLens = Optic
{-# INLINE mkLens #-}

-- | Build a lens from a getter and setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set =
  mkLens (dimap (\ s -> (set s, get s)) (uncurry ($)) . second')
{-# INLINE lens #-}
