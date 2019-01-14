module Optics.Internal.IxFold where

import Data.Monoid

import Optics.Internal.Bi
import Optics.Internal.Indexed
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils

-- | Type synonym for an indexed fold.
type IxFold i o s a = Optic' A_Fold i o s a

-- | Explicitly cast an optic to an indexed fold.
toIxFold :: Is k A_Fold => Optic' k i o s a -> IxFold i o s a
toIxFold = sub
{-# INLINE toIxFold #-}

-- | Fold with index via embedding into a monoid.
ifoldMapOf
  :: (CheckIndices i o, Monoid r, Is k A_Fold)
  => Optic' k i o s a
  -> (i -> a -> r) -> s -> r
ifoldMapOf o f = runIxForget (getOptic (toIxFold o) (IxForget f)) id
{-# INLINE ifoldMapOf #-}

-- | Fold with index right-associatively.
ifoldrOf
  :: (CheckIndices i o, Is k A_Fold)
  => Optic' k i o s a
  -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf o iarr r0 = (\e -> appEndo e r0) . ifoldMapOf o (\i -> Endo #. iarr i)
{-# INLINE ifoldrOf #-}

-- | Fold with index left-associatively, and strictly.
ifoldlOf'
  :: (CheckIndices i o, Is k A_Fold)
  => Optic' k i o s a
  -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' o irar r0 s = ifoldrOf o (\i a rr r -> rr $! irar i r a) id s r0
{-# INLINE ifoldlOf' #-}

-- | Fold with index to a list.
--
-- >>> itoListOf (each % ifolded) ("abc", "def")
-- [(0,'a'),(1,'b'),(2,'c'),(0,'d'),(1,'e'),(2,'f')]
--
-- /Note:/ currently indexed optics can be used as non-indexed
--
-- >>> toListOf (each % ifolded) ("abc", "def")
-- "abcdef"
--
itoListOf
  :: (CheckIndices i o, Is k A_Fold)
  => Optic' k i o s a
  -> s -> [(i, a)]
itoListOf o = ifoldrOf o (\i -> (:) . (i, )) []
{-# INLINE itoListOf #-}

----------------------------------------

itraverseOf_
  :: ( CheckIndices i o, Is k A_Fold, Applicative f)
  => Optic' k i o s a
  -> (i -> a -> f r) -> s -> f ()
itraverseOf_ o f = ifoldrOf o (\i -> (*>) . f i) (pure ())
{-# INLINE itraverseOf_ #-}

----------------------------------------

-- | Indexed fold via 'FoldableWithIndex' class.
ifolded :: FoldableWithIndex i f => IxFold j (i -> j) (f a) a
ifolded = Optic (contrasecond (\_ -> ()) . iwander itraverse_)
{-# INLINE ifolded #-}

-- $setup
-- >>> import Optics
