module Optics.Internal.IxFold where

import Data.Functor
import Data.Foldable

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Profunctor
import Optics.Internal.Optic

-- | Internal implementation of 'ixFoldVL'.
ixFoldVL__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (i -> a -> f r) -> s -> f ())
  -> Optic__ p j (i -> j) s t a b
ixFoldVL__ f = rphantom . iwander f . rphantom
{-# INLINE ixFoldVL__ #-}

-- | Internal implementation of 'ifolded'.
ifolded__
  :: (Bicontravariant p, Traversing p, FoldableWithIndex i f)
  => Optic__ p j (i -> j) (f a) (f b) a b
ifolded__ = conjoinedFold__ traverse_ itraverse_
{-# INLINE ifolded__ #-}

-- | Internal implementation of 'ifoldring'.
ifoldring__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (i -> a -> f r -> f r) -> f r -> s -> f r)
  -> Optic__ p j (i -> j) s t a b
ifoldring__ fr = ixFoldVL__ $ \f -> void . fr (\i a -> (f i a *>)) (pure v)
  where
    v = error "ifoldring__: value used"
{-# INLINE ifoldring__ #-}

-- | Internal implementation of 'conjoinedFold'.
conjoinedFold__
  :: (Bicontravariant p, Traversing p)
  => (forall f. Applicative f => (     a -> f r) -> s -> f ())
  -> (forall f. Applicative f => (i -> a -> f r) -> s -> f ())
  -> Optic__ p j (i -> j) s t a b
conjoinedFold__ f g = conjoined (rphantom . wander  f . rphantom)
                                (rphantom . iwander g . rphantom)
{-# INLINE conjoinedFold__ #-}
