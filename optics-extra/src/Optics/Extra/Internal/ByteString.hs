-- | This module spends a lot of time fiddling around with 'Data.ByteString'
-- internals to work around <http://hackage.haskell.org/trac/ghc/ticket/7556> on
-- older Haskell Platforms and to improve constant and asymptotic factors in our
-- performance.
----------------------------------------------------------------------------
module Optics.Extra.Internal.ByteString
  ( traversedStrictTree
  , traversedStrictTree8
  , traversedLazy
  , traversedLazy8
  ) where

import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Base (unsafeChr)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import GHC.IO (unsafeDupablePerformIO)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Internal   as BI
import qualified Data.ByteString.Unsafe     as BU

import Optics.Core
import Optics.Internal.Fold
import Optics.Internal.IxFold
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a
-- balanced tree with biased runs of elements at the leaves.
traversedStrictTree :: IxTraversal' Int B.ByteString Word8
traversedStrictTree = Optic traversedStrictTree__
{-# INLINE traversedStrictTree #-}

-- | Traverse a strict 'B.ByteString' in a relatively balanced fashion, as a
-- balanced tree with biased runs of elements at the leaves, pretending the
-- bytes are chars.
traversedStrictTree8 :: IxTraversal' Int B8.ByteString Char
traversedStrictTree8 = Optic traversedStrictTree8__
{-# INLINE traversedStrictTree8 #-}

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString'.
traversedLazy :: IxTraversal' Int64 BL.ByteString Word8
traversedLazy = Optic traversedLazy__
{-# INLINE traversedLazy #-}

-- | An 'IndexedTraversal' of the individual bytes in a lazy 'BL.ByteString'
-- pretending the bytes are chars.
traversedLazy8 :: IxTraversal' Int64 BL.ByteString Char
traversedLazy8 = Optic traversedLazy8__
{-# INLINE traversedLazy8 #-}

----------------------------------------
-- Internal implementations

grain :: Int
grain = 32
{-# INLINE grain #-}

-- | Internal version of 'traversedStrictTree'.
traversedStrictTree__
  :: Traversing p
  => Optic__ p j (Int -> j) B.ByteString B.ByteString Word8 Word8
traversedStrictTree__ = iwander $ \f bs ->
  let len = B.length bs
      go !i !j
        | i + grain < j, k <- i + shiftR (j - i) 1 =
            (\l r q -> l q >> r q) <$> go i k <*> go k j
        | otherwise = run i j
      run !i !j
        | i == j    = pure (\_ -> return ())
        | otherwise =
          let !x = BU.unsafeIndex bs i
          in (\y ys q -> pokeByteOff q i y >> ys q)
               <$> f i x
               <*> run (i + 1) j
  in unsafeCreate len <$> go 0 len
{-# INLINE [0] traversedStrictTree__ #-}

{-# RULES

"bytes -> map"
  forall (o :: FunArrow j Word8 Word8). traversedStrictTree__ o
                                      = roam B.map (FunArrow (runFunArrow o))
    :: FunArrow (Int -> j) B.ByteString B.ByteString

"bytes -> imap"
  forall (o :: IxFunArrow j Word8 Word8). traversedStrictTree__ o = iroam imapB o
    :: IxFunArrow (Int -> j) B.ByteString B.ByteString

"bytes -> foldr"
  forall (o :: Forget r j Word8 Word8). traversedStrictTree__ o
                                      = foldring__ B.foldr (Forget (runForget o))
    :: Forget r (Int -> j) B.ByteString B.ByteString

"bytes -> ifoldr"
  forall (o :: IxForget r j Word8 Word8). traversedStrictTree__ o
                                        = ifoldring__ ifoldrB o
    :: IxForget r (Int -> j) B.ByteString B.ByteString

#-}

-- | Indexed setter for 'traversedStrictTree__'.
imapB :: (Int -> Word8 -> Word8) -> B.ByteString -> B.ByteString
imapB f = snd . B.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapB #-}

-- | Indexed fold for 'traversedStrictTree__'.
ifoldrB :: (Int -> Word8 -> a -> a) -> a -> B.ByteString -> a
ifoldrB f z xs = B.foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z) xs 0
{-# INLINE ifoldrB #-}

----------------------------------------

-- | Internal version of 'traversedStrictTree8'.
traversedStrictTree8__
  :: Traversing p
  => Optic__ p j (Int -> j) B8.ByteString B8.ByteString Char Char
traversedStrictTree8__ = iwander $ \f bs ->
  let len = B.length bs
      go !i !j
        | i + grain < j, k <- i + shiftR (j - i) 1 =
            (\l r q -> l q >> r q) <$> go i k <*> go k j
        | otherwise = run i j
      run !i !j
        | i == j    = pure (\_ -> return ())
        | otherwise =
          let !x = BU.unsafeIndex bs i
          in (\y ys q -> pokeByteOff q i (c2w y) >> ys q)
               <$> f i (w2c x)
               <*> run (i + 1) j
  in unsafeCreate len <$> go 0 len
{-# INLINE [0] traversedStrictTree8__ #-}

{-# RULES

"chars -> map"
  forall (o :: FunArrow j Char Char). traversedStrictTree8__ o
                                    = roam B8.map (FunArrow (runFunArrow o))
    :: FunArrow (Int -> j) B8.ByteString B8.ByteString

"chars -> imap"
  forall (o :: IxFunArrow j Char Char). traversedStrictTree8__ o = iroam imapB8 o
    :: IxFunArrow (Int -> j) B8.ByteString B8.ByteString

"chars -> foldr"
  forall (o :: Forget r j Char Char). traversedStrictTree8__ o
                                    = foldring__ B8.foldr (Forget (runForget o))
    :: Forget r (Int -> j) B8.ByteString B8.ByteString

"chars -> ifoldr"
  forall (o :: IxForget r j Char Char). traversedStrictTree8__ o
                                      = ifoldring__ ifoldrB8 o
    :: IxForget r (Int -> j) B8.ByteString B8.ByteString

#-}

-- | Indexed setter for 'traversedStrictTree8__'.
imapB8 :: (Int -> Char -> Char) -> B.ByteString -> B.ByteString
imapB8 f = snd . B8.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapB8 #-}

-- | Indexed fold for 'traversedStrictTree8__'.
ifoldrB8 :: (Int -> Char -> a -> a) -> a -> B.ByteString -> a
ifoldrB8 f z xs = B8.foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z) xs 0
{-# INLINE ifoldrB8 #-}

----------------------------------------

-- | Internal version of 'traversedLazy'.
traversedLazy__
  :: Traversing p
  => Optic__ p j (Int64 -> j) BL.ByteString BL.ByteString Word8 Word8
traversedLazy__ = iwander $ \f lbs ->
  let go c fcs acc =
        let !acc' = acc + fromIntegral (B.length c)
            rest = reindex (\x -> acc + fromIntegral x) traversedStrictTree
        in BL.append . BL.fromStrict <$> itraverseOf rest f c <*> fcs acc'
  in BL.foldrChunks go (\_ -> pure BL.empty) lbs 0
{-# INLINE [1] traversedLazy__ #-}

{-# RULES

"sets lazy bytestring"
  forall (o :: FunArrow j Word8 Word8). traversedLazy__ o
                                      = roam BL.map (FunArrow (runFunArrow o))
    :: FunArrow (Int64 -> j) BL.ByteString BL.ByteString

"isets lazy bytestring"
  forall (o :: IxFunArrow j Word8 Word8). traversedLazy__ o = iroam imapBL o
    :: IxFunArrow (Int64 -> j) BL.ByteString BL.ByteString

"gets lazy bytestring"
  forall (o :: Forget r j Word8 Word8). traversedLazy__ o
                                      = foldring__ BL.foldr (Forget (runForget o))
    :: Forget r (Int64 -> j) BL.ByteString BL.ByteString

"igets lazy bytestring"
  forall (o :: IxForget r j Word8 Word8). traversedLazy__ o = ifoldring__ ifoldrBL o
    :: IxForget r (Int64 -> j) BL.ByteString BL.ByteString

#-}

-- | Indexed setter for 'traversedLazy__'.
imapBL :: (Int64 -> Word8 -> Word8) -> BL.ByteString -> BL.ByteString
imapBL f = snd . BL.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapBL #-}

-- | Indexed fold for 'traversedLazy__'.
ifoldrBL :: (Int64 -> Word8 -> a -> a) -> a -> BL.ByteString -> a
ifoldrBL f z xs = BL.foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z) xs 0
{-# INLINE ifoldrBL #-}

----------------------------------------

-- | Internal version of 'traversedLazy8'.
traversedLazy8__
  :: Traversing p
  => Optic__ p j (Int64 -> j) BL.ByteString BL.ByteString Char Char
traversedLazy8__ = iwander $ \f lbs ->
  let go c fcs acc =
        let !acc' = acc + fromIntegral (B.length c)
            rest = reindex (\x -> acc + fromIntegral x) traversedStrictTree8
        in BL.append . BL.fromStrict <$> itraverseOf rest f c <*> fcs acc'
  in BL.foldrChunks go (\_ -> pure BL.empty) lbs 0
{-# INLINE [1] traversedLazy8__ #-}

{-# RULES

"sets lazy char bytestring"
  forall (o :: FunArrow j Char Char). traversedLazy8__ o
                                    = roam BL8.map (FunArrow (runFunArrow o))
    :: FunArrow (Int64 -> j) BL8.ByteString BL8.ByteString

"isets lazy char bytestring"
  forall (o :: IxFunArrow j Char Char). traversedLazy8__ o = iroam imapBL8 o
    :: IxFunArrow (Int64 -> j) BL8.ByteString BL8.ByteString

"gets lazy char bytestring"
  forall (o :: Forget r j Char Char). traversedLazy8__ o
                                    = foldring__ BL8.foldr (Forget (runForget o))
    :: Forget r (Int64 -> j) BL8.ByteString BL8.ByteString

"igets lazy char bytestring"
  forall (o :: IxForget r j Char Char). traversedLazy8__ o = ifoldring__ ifoldrBL8 o
    :: IxForget r (Int64 -> j) BL.ByteString BL.ByteString

#-}

-- | Indexed setter for 'traversedLazy8__'.
imapBL8 :: (Int64 -> Char -> Char) -> BL8.ByteString -> BL8.ByteString
imapBL8 f = snd . BL8.mapAccumL (\i a -> i `seq` (i + 1, f i a)) 0
{-# INLINE imapBL8 #-}

-- | Indexed fold for 'traversedLazy8__'.
ifoldrBL8 :: (Int64 -> Char -> a -> a) -> a -> BL8.ByteString -> a
ifoldrBL8 f z xs = BL8.foldr (\x g i -> i `seq` f i x (g (i + 1))) (const z) xs 0
{-# INLINE ifoldrBL8 #-}

------------------------------------------------------------------------------
-- ByteString guts
------------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and silently
-- truncates to 8 bits Chars > '\255'. It is provided as convenience for
-- ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | A way of creating ByteStrings outside the IO monad. The @Int@ argument
-- gives the final size of the ByteString. Unlike 'createAndTrim' the ByteString
-- is not reallocated if the final size is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> B.ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO B.ByteString
create l f = do
    fp <- mallocPlainForeignPtrBytes l
    withForeignPtr fp $ \p -> f p
    return $! BI.PS fp 0 l
{-# INLINE create #-}
