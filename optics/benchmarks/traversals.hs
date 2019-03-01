{-# LANGUAGE CPP #-}
module Main where

import Criterion.Main
import Criterion.Types
import Data.Char
import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.ByteString.Optics
import Optics

seqTraverseWithIndex
  :: Applicative f => (Int -> a -> f b) -> S.Seq a -> f (S.Seq b)
seqTraverseWithIndex f =
#if MIN_VERSION_containers(0,5,8)
  S.traverseWithIndex f
#else
  sequenceA . S.mapWithIndex f
#endif

main :: IO ()
main = defaultMainWith config
  [ bgroup "vector"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) v
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) v
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) v
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (traverse (\(i, a) -> S.modify' $ (i + a +)) $ V.indexed x) 0) v
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) v
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) v
      ]
    , bgroup "map"
      [ bench "native"     $ nf (V.map (+100)) v
      , bench "itraversed" $ nf (over itraversed (+100)) v
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (V.imap           (\i x -> x + i +100)) v
      , bench "imap"       $ nf (imap             (\i x -> x + i +100)) v
      , bench "itraversed" $ nf (iover itraversed (\i x -> x + i +100)) v
      ]
    ]
  , bgroup "unboxed-vector"
    [ bgroup "map"
      [ bench "native"     $ nf (U.map (+100)) u
      , bench "itraversed" $ nf (over each (+100)) u
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (U.imap (\i x -> x + i +100)) u
      , bench "itraversed" $ nf (iover each (\i x -> x + i)) u
      ]
    ]
  , bgroup "sequence"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) s
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) s
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (seqTraverseWithIndex (\i a -> S.modify' $ (i + a +)) x) 0) s
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) s
      ]
    , bgroup "map"
      [ bench "native" $ nf (fmap            (+100)) s
      , bench "each"   $ nf (over each       (+100)) s
      ]
    , bgroup "imap"
      [ bench "native" $ nf (S.mapWithIndex    (\i x -> x + i +100)) s
      , bench "imap"   $ nf (imap              (\i x -> x + i +100)) s
      ]
    ]
  , bgroup "bytestring"
    [ bgroup "map"
      [ bench "native" $ nf (BS.map     (+100)) b
      , bench "each"   $ nf (over each  (+100)) b
      ]
    , bgroup "imap"
      [ bench "bytes" $ nf (iover bytes (\i x -> x + fromIntegral i)) b
      ]
    ]
  , bgroup "bytestring char8"
    [ bgroup "map"
      [ bench "native" $ nf (BS8.map     (chr . (+100) . ord)) b
      , bench "chars"  $ nf (over chars  (chr . (+100) . ord)) b
      ]
    , bgroup "imap"
      [ bench "chars" $ nf
          (iover chars (\i x -> chr $ ord x + fromIntegral (i `mod` 256))) b
      ]
    ]
  , bgroup "bytestring lazy"
    [ bgroup "map"
      [ bench "native" $ nf (BSL.map    (+100)) bl
      , bench "each"   $ nf (over each  (+100)) bl
      ]
    , bgroup "imap"
      [ bench "bytes" $ nf (iover bytes (\i x -> x + fromIntegral i)) bl
      ]
    ]
  , bgroup "bytestring lazy char8"
    [ bgroup "map"
      [ bench "native" $ nf (BSL8.map    (chr . (+100) . ord)) bl
      , bench "chars"  $ nf (over chars  (chr . (+100) . ord)) bl
      ]
    , bgroup "imap"
      [ bench "chars" $ nf
          (iover chars (\i x -> chr $ ord x + fromIntegral (i `mod` 256))) bl
      ]
    ]
  , bgroup "list"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) l
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) l
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) l
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (traverse (\(i, a) -> S.modify' $ (i + a +)) (zip [0..] x)) 0) l
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) l
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) l
      ]
    , bgroup "map"
      [ bench "native" $ nf (map       (+100)) l
      , bench "each"   $ nf (over each (+100)) l
      ]
    , bgroup "imap"
      [ bench "imap" $ nf (imap (\i x -> x + i +100)) l
      ]
    ]
  , bgroup "intmap"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) im
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) im
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) im
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (IM.traverseWithKey (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) im
      ]
    , bgroup "map"
      [ bench "native"     $ nf (fmap            (+100)) im
      , bench "each"       $ nf (over each       (+100)) im
      , bench "itraversed" $ nf (over itraversed (+100)) im
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (IM.mapWithKey    (\i x -> x + i +100)) im
      , bench "each"       $ nf (iover each       (\i x -> x + i +100)) im
      , bench "itraversed" $ nf (iover itraversed (\i x -> x + i +100)) im
      ]
    ]
  , bgroup "map"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) m
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) m
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) m
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (M.traverseWithKey (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) m
      ]
    , bgroup "map"
      [ bench "native"     $ nf (fmap            (+100)) m
      , bench "each"       $ nf (over each       (+100)) m
      , bench "itraversed" $ nf (over itraversed (+100)) m
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (M.mapWithKey     (\i x -> x + i +100)) m
      , bench "each"       $ nf (iover each       (\i x -> x + i +100)) m
      , bench "itraversed" $ nf (iover itraversed (\i x -> x + i +100)) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) h
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) h
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) h
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (HM.traverseWithKey (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) h
      ]
    , bgroup "map"
      [ bench "native"     $ nf (HM.map          (+100)) h
      , bench "each"       $ nf (over each       (+100)) h
      , bench "itraversed" $ nf (over itraversed (+100)) h
      ]
    , bgroup "imap"
      [ bench "native"     $ nf (HM.mapWithKey    (\i x -> x + i +100)) h
      , bench "each"       $ nf (iover each       (\i x -> x + i +100)) h
      , bench "itraversed" $ nf (iover itraversed (\i x -> x + i +100)) h
      ]
    ]
  ]
  where
    config = defaultConfig { timeLimit = 1 }
    l  = [0..10000] :: [Int]
    xl = [0..100000] :: [Int]
    b  = BS.pack $ map fromIntegral xl
    bl = BSL.pack $ map fromIntegral [0..1000000::Int]
    h  = HM.fromList $ zip l l
    m  = M.fromList $ zip l l
    im = IM.fromList $ zip l l
    s  = S.fromList l
    u  = U.fromList xl
    v  = V.fromList l
