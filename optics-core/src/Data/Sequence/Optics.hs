-- |
-- Module:  Data.Sequence.Optics
-- Description: Optics for working with 'Seq's.
--
-- This module defines optics for constructing and manipulating finite 'Seq's.
--
module Data.Sequence.Optics
  ( viewL, viewR
  , sliced, slicedTo, slicedFrom
  , seqOf
  ) where

import Data.Sequence as Seq

import Optics.Internal.Indexed
import Optics.Fold
import Optics.Iso
import Optics.IxTraversal
import Optics.Traversal

-- * Sequence isomorphisms

-- | A 'Seq' is isomorphic to a 'ViewL'
--
-- @'viewl' m ≡ m '^.' 'viewL'@
--
-- >>> Seq.fromList [1,2,3] ^. viewL
-- 1 :< fromList [2,3]
--
-- >>> Seq.empty ^. viewL
-- EmptyL
--
-- >>> EmptyL ^. re viewL
-- fromList []
--
-- >>> review viewL $ 1 Seq.:< fromList [2,3]
-- fromList [1,2,3]
viewL :: Iso (Seq a) (Seq b) (ViewL a) (ViewL b)
viewL = iso viewl $ \xs -> case xs of
  EmptyL      -> mempty
  a Seq.:< as -> a Seq.<| as
{-# INLINE viewL #-}

-- | A 'Seq' is isomorphic to a 'ViewR'
--
-- @'viewr' m ≡ m '^.' 'viewR'@
--
-- >>> Seq.fromList [1,2,3] ^. viewR
-- fromList [1,2] :> 3
--
-- >>> Seq.empty ^. viewR
-- EmptyR
--
-- >>> EmptyR ^. re viewR
-- fromList []
--
-- >>> review viewR $ fromList [1,2] Seq.:> 3
-- fromList [1,2,3]
viewR :: Iso (Seq a) (Seq b) (ViewR a) (ViewR b)
viewR = iso viewr $ \xs -> case xs of
  EmptyR      -> mempty
  as Seq.:> a -> as Seq.|> a
{-# INLINE viewR #-}

-- | Traverse the first @n@ elements of a 'Seq'
--
-- >>> fromList [1,2,3,4,5] ^.. slicedTo 2
-- [1,2]
--
-- >>> fromList [1,2,3,4,5] & slicedTo 2 %~ (*10)
-- fromList [10,20,3,4,5]
--
-- >>> fromList [1,2,4,5,6] & slicedTo 10 .~ 0
-- fromList [0,0,0,0,0]
slicedTo :: Int -> IxTraversal' Int (Seq a) a
slicedTo n = conjoined noix ix
  where
    noix = traversalVL $ \f m -> case Seq.splitAt n m of
      (l, r) -> (>< r) <$> traverse f l

    ix = ixTraversalVL $ \f m -> case Seq.splitAt n m of
      (l, r) -> (>< r) <$> itraverse f l
{-# INLINE slicedTo #-}

-- | Traverse all but the first @n@ elements of a 'Seq'
--
-- >>> fromList [1,2,3,4,5] ^.. slicedFrom 2
-- [3,4,5]
--
-- >>> fromList [1,2,3,4,5] & slicedFrom 2 %~ (*10)
-- fromList [1,2,30,40,50]
--
-- >>> fromList [1,2,3,4,5] & slicedFrom 10 .~ 0
-- fromList [1,2,3,4,5]
slicedFrom :: Int -> IxTraversal' Int (Seq a) a
slicedFrom n = conjoined noix ix
  where
    noix = traversalVL $ \f m -> case Seq.splitAt n m of
      (l, r) -> (l ><) <$> traverse f r

    ix = ixTraversalVL $ \f m -> case Seq.splitAt n m of
      (l, r) -> (l ><) <$> itraverse (f . (+n)) r
{-# INLINE slicedFrom #-}

-- | Traverse all the elements numbered from @i@ to @j@ of a 'Seq'
--
-- >>> fromList [1,2,3,4,5] & sliced 1 3 %~ (*10)
-- fromList [1,20,30,4,5]
--
-- >>> fromList [1,2,3,4,5] ^.. sliced 1 3
-- [2,3]
--
-- >>> fromList [1,2,3,4,5] & sliced 1 3 .~ 0
-- fromList [1,0,0,4,5]
sliced :: Int -> Int -> IxTraversal' Int (Seq a) a
sliced i j = conjoined noix ix
  where
    noix = traversalVL $ \f s -> case Seq.splitAt i s of
      (l, mr) -> case Seq.splitAt (j-i) mr of
        (m, r) -> traverse f m <&> \n -> l >< n >< r

    ix = ixTraversalVL $ \f s -> case Seq.splitAt i s of
      (l, mr) -> case Seq.splitAt (j-i) mr of
        (m, r) -> itraverse (f . (+i)) m <&> \n -> l >< n >< r
{-# INLINE sliced #-}

-- | Construct a 'Seq' from a fold.
--
-- >>> seqOf folded ["hello","world"]
-- fromList ["hello","world"]
--
-- >>> seqOf (folded % _2) [("hello",1),("world",2),("!!!",3)]
-- fromList [1,2,3]
seqOf :: Is k A_Fold => Optic' k is s a -> s -> Seq a
seqOf l = foldMapOf l Seq.singleton
{-# INLINE seqOf #-}

-- $setup
-- >>> import Optics.Core
