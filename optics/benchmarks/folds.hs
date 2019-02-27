module Main where

import Criterion.Main
import Criterion.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.ByteString.Optics
import Optics

itoList :: FoldableWithIndex i f => f a -> [(i, a)]
itoList = ifoldr (\i c -> ((i, c) :)) []

main :: IO ()
main = defaultMainWith config
  [ bgroup "vector"
    [ bgroup "toList"
      [ bench "native" $ nf V.toList v
      , bench "each"   $ nf (toListOf each) v
      ]
    , bgroup "itoList"
      [ bench "native"  $ nf (V.toList . V.indexed) v
      , bench "itoList" $ nf itoList v
      , bench "each"    $ nf (itoListOf each) v
      ]
    ]
  , bgroup "unboxed-vector"
    [ bgroup "toList"
      [ bench "native" $ nf U.toList u
      , bench "each"   $ nf (toListOf each) u
      ]
    , bgroup "itoList"
      [ bench "native" $ nf (U.toList . U.indexed) u
      , bench "each"   $ nf (itoListOf each) u
      ]
    ]
  , bgroup "sequence"
    [ bgroup "toList"
      [ bench "native" $ nf F.toList s
      , bench "each"   $ nf (toListOf each) s
      ]
    , bgroup "itoList"
      [ bench "native"  $ nf (F.toList . S.mapWithIndex (,)) s
      , bench "itoList" $ nf itoList s
      , bench "each"    $ nf (itoListOf each) s
      ]
    ]
  , bgroup "bytestring"
    [ bgroup "toList"
      [ bench "native" $ nf BS.unpack b
      , bench "bytes"  $ nf (toListOf bytes) b
      , bench "each"   $ nf (toListOf each) b
      ]
    , bgroup "itoList"
      [ bench "native" $ nf (zip [(0::Int)..] . BS.unpack) b
      , bench "bytes"  $ nf (itoListOf bytes) b
      , bench "each"   $ nf (itoListOf each) b
      ]
    ]
  , bgroup "bytestring lazy"
    [ bgroup "toList"
      [ bench "native" $ nf BSL.unpack bl
      , bench "bytes"  $ nf (toListOf bytes) bl
      , bench "each"   $ nf (toListOf each) bl
      ]
    , bgroup "itoList"
      [ bench "native"  $ nf (zip [(0::Int)..] . BSL.unpack) bl
      , bench "bytes"   $ nf (itoListOf bytes) bl
      , bench "each"    $ nf (itoListOf each) bl
      ]
    ]
  , bgroup "list"
    [ bgroup "toList"
      [ bench "native"     $ nf F.toList l
      , bench "each"       $ nf (toListOf each) l
      , bench "itraversed" $ nf (toListOf itraversed) l
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..]) l
      , bench "itoList"    $ nf itoList l
      , bench "each"       $ nf (itoListOf each) l
      , bench "itraversed" $ nf (itoListOf itraversed) l
      ]
    ]
  , bgroup "intmap"
    [ bgroup "toList"
      [ bench "native"     $ nf F.toList im
      , bench "each"       $ nf (toListOf each) im
      , bench "itraversed" $ nf (toListOf itraversed) im
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf IM.toList im
      , bench "itoList"    $ nf itoList im
      , bench "each"       $ nf (itoListOf each) im
      , bench "itraversed" $ nf (itoListOf itraversed) im
      ]
    ]
  , bgroup "map"
    [ bgroup "toList"
      [ bench "native"     $ nf F.toList m
      , bench "each"       $ nf (toListOf each) m
      , bench "itraversed" $ nf (toListOf itraversed) m
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf M.toList m
      , bench "itoList"    $ nf itoList m
      , bench "each"       $ nf (itoListOf each) m
      , bench "itraversed" $ nf (itoListOf itraversed) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "toList"
      [ bench "native"     $ nf HM.keys h
      , bench "each"       $ nf (toListOf each) h
      , bench "itraversed" $ nf (toListOf itraversed) h
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf HM.toList h
      , bench "itoList"    $ nf itoList h
      , bench "each"       $ nf (itoListOf each) h
      , bench "itraversed" $ nf (itoListOf itraversed) h
      ]
    , bgroup "sum"
      [ bench "native"     $ nf (sum . F.toList) h
      , bench "each"       $ nf (sumOf each) h
      , bench "itraversed" $ nf (sumOf itraversed) h
      ]
    ]
  ]
  where
    config = defaultConfig { timeLimit = 1 }
    l  = [0..10000] :: [Int]
    b  = BS.pack $ map fromIntegral l
    bl = BSL.pack $ map fromIntegral [0..1000000::Int]
    h  = HM.fromList $ zip l l
    m  = M.fromList $ zip l l
    im = IM.fromList $ zip l l
    s  = S.fromList l
    u  = U.fromList l
    v  = V.fromList l
