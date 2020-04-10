module Main where

import Criterion.Main
import Criterion.Types
import qualified Control.Lens as L
import qualified Data.ByteString.Lens as L
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

main :: IO ()
main = defaultMainWith config
  [ bgroup "vector"
    [ bgroup "toList"
      [ bench "native"          $ nf V.toList v
      , bench "each"            $ nf (toListOf each) v
      , bench "each/lens"       $ nf (L.toListOf L.each) v
      , bench "itraversed"      $ nf (toListOf itraversed) v
      , bench "itraversed/lens" $ nf (L.toListOf L.itraversed) v
      , bench "ifolded"         $ nf (toListOf ifolded) v
      , bench "ifolded/lens"    $ nf (L.toListOf L.ifolded) v
      ]
    , bgroup "itoList"
      [ bench "native"          $ nf (V.toList . V.indexed) v
      , bench "itoList"         $ nf itoList v
      , bench "itoList/lens"    $ nf L.itoList v
      , bench "each"            $ nf (itoListOf each) v
      , bench "itraversed"      $ nf (itoListOf itraversed) v
      , bench "itraversed/lens" $ nf (L.itoListOf L.itraversed) v
      , bench "ifolded"         $ nf (itoListOf ifolded) v
      , bench "ifolded/lens"    $ nf (L.itoListOf L.ifolded) v
      ]
    ]
  , bgroup "unboxed-vector"
    [ bgroup "toList"
      [ bench "native"    $ nf U.toList u
      , bench "each"      $ nf (toListOf each) u
      , bench "each/lens" $ nf (L.toListOf L.each) u
      ]
    , bgroup "itoList"
      [ bench "native" $ nf (U.toList . U.indexed) u
      , bench "each"   $ nf (itoListOf each) u
      ]
    ]
  , bgroup "sequence"
    [ bgroup "toList"
      [ bench "native"          $ nf F.toList s
      , bench "each"            $ nf (toListOf each) s
      , bench "each/lens"       $ nf (L.toListOf L.each) s
      , bench "itraversed"      $ nf (toListOf itraversed) s
      , bench "itraversed/lens" $ nf (L.toListOf L.itraversed) s
      , bench "ifolded"         $ nf (toListOf ifolded) s
      , bench "ifolded/lens"    $ nf (L.toListOf L.ifolded) s
      ]
    , bgroup "itoList"
      [ bench "native"          $ nf (F.toList . S.mapWithIndex (,)) s
      , bench "itoList"         $ nf itoList s
      , bench "itoList/lens"    $ nf L.itoList s
      , bench "each"            $ nf (itoListOf each) s
      , bench "itraversed"      $ nf (itoListOf itraversed) s
      , bench "itraversed/lens" $ nf (L.itoListOf L.itraversed) s
      , bench "ifolded"         $ nf (itoListOf ifolded) s
      , bench "ifolded/lens"    $ nf (L.itoListOf L.ifolded) s
      ]
    ]
  , bgroup "bytestring"
    [ bgroup "toList"
      [ bench "native"     $ nf BS.unpack b
      , bench "bytes"      $ nf (toListOf bytes) b
      , bench "bytes/lens" $ nf (L.toListOf L.bytes) b
      , bench "each"       $ nf (toListOf each) b
      , bench "each/lens"  $ nf (L.toListOf L.each) b
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..] . BS.unpack) b
      , bench "bytes"      $ nf (itoListOf bytes) b
      , bench "bytes/lens" $ nf (L.itoListOf L.bytes) b
      , bench "each"       $ nf (itoListOf each) b
      ]
    ]
  , bgroup "bytestring lazy"
    [ bgroup "toList"
      [ bench "native"     $ nf BSL.unpack bl
      , bench "bytes"      $ nf (toListOf bytes) bl
      , bench "bytes/lens" $ nf (L.toListOf L.bytes) bl
      , bench "each"       $ nf (toListOf each) bl
      , bench "each/lens"  $ nf (L.toListOf L.each) bl
      ]
    , bgroup "itoList"
      [ bench "native"     $ nf (zip [(0::Int)..] . BSL.unpack) bl
      , bench "bytes"      $ nf (itoListOf bytes) bl
      , bench "bytes/lens" $ nf (L.itoListOf L.bytes) bl
      , bench "each"       $ nf (itoListOf each) bl
      ]
    ]
  , bgroup "list"
    [ bgroup "toList"
      [ bench "native"          $ nf F.toList l
      , bench "each"            $ nf (toListOf each) l
      , bench "each/lens"       $ nf (L.toListOf L.each) l
      , bench "itraversed"      $ nf (toListOf itraversed) l
      , bench "itraversed/lens" $ nf (L.toListOf L.itraversed) l
      , bench "ifolded"         $ nf (toListOf ifolded) l
      , bench "ifolded/lens"    $ nf (L.toListOf L.ifolded) l
      ]
    , bgroup "itoList"
      [ bench "native"          $ nf (zip [(0::Int)..]) l
      , bench "itoList"         $ nf itoList l
      , bench "itoList/lens"    $ nf L.itoList l
      , bench "each"            $ nf (itoListOf each) l
      , bench "itraversed"      $ nf (itoListOf itraversed) l
      , bench "itraversed/lens" $ nf (L.itoListOf L.itraversed) l
      , bench "ifolded"         $ nf (itoListOf ifolded) l
      , bench "ifolded/lens"    $ nf (L.itoListOf L.ifolded) l
      ]
    ]
  , bgroup "intmap"
    [ bgroup "toList"
      [ bench "native"          $ nf F.toList im
      , bench "each"            $ nf (toListOf each) im
      , bench "each/lens"       $ nf (L.toListOf L.each) im
      , bench "itraversed"      $ nf (toListOf itraversed) im
      , bench "itraversed/lens" $ nf (L.toListOf L.itraversed) im
      , bench "ifolded"         $ nf (toListOf ifolded) im
      , bench "ifolded/lens"    $ nf (L.toListOf L.ifolded) im
      ]
    , bgroup "itoList"
      [ bench "native"          $ nf IM.toList im
      , bench "itoList"         $ nf itoList im
      , bench "itoList/lens"    $ nf L.itoList im
      , bench "each"            $ nf (itoListOf each) im
      , bench "itraversed"      $ nf (itoListOf itraversed) im
      , bench "itraversed/lens" $ nf (L.itoListOf L.itraversed) im
      , bench "ifolded"         $ nf (itoListOf ifolded) im
      , bench "ifolded/lens"    $ nf (L.itoListOf L.ifolded) im
      ]
    ]
  , bgroup "map"
    [ bgroup "toList"
      [ bench "native"          $ nf F.toList m
      , bench "each"            $ nf (toListOf each) m
      , bench "each/lens"       $ nf (L.toListOf L.each) m
      , bench "itraversed"      $ nf (toListOf itraversed) m
      , bench "itraversed/lens" $ nf (L.toListOf L.itraversed) m
      , bench "ifolded"         $ nf (toListOf ifolded) m
      , bench "ifolded/lens"    $ nf (L.toListOf L.ifolded) m
      ]
    , bgroup "itoList"
      [ bench "native"          $ nf M.toList m
      , bench "itoList"         $ nf itoList m
      , bench "itoList/lens"    $ nf L.itoList m
      , bench "each"            $ nf (itoListOf each) m
      , bench "itraversed"      $ nf (itoListOf itraversed) m
      , bench "itraversed/lens" $ nf (L.itoListOf L.itraversed) m
      , bench "ifolded"         $ nf (itoListOf ifolded) m
      , bench "ifolded/lens"    $ nf (L.itoListOf L.ifolded) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "toList"
      [ bench "native"          $ nf HM.keys h
      , bench "each"            $ nf (toListOf each) h
      , bench "each/lens"       $ nf (L.toListOf L.each) h
      , bench "itraversed"      $ nf (toListOf itraversed) h
      , bench "itraversed/lens" $ nf (L.toListOf L.itraversed) h
      , bench "ifolded"         $ nf (toListOf ifolded) h
      , bench "ifolded/lens"    $ nf (L.toListOf L.ifolded) h
      ]
    , bgroup "itoList"
      [ bench "native"          $ nf HM.toList h
      , bench "itoList"         $ nf itoList h
      , bench "itoList/lens"    $ nf L.itoList h
      , bench "each"            $ nf (itoListOf each) h
      , bench "itraversed"      $ nf (itoListOf itraversed) h
      , bench "itraversed/lens" $ nf (L.itoListOf L.itraversed) h
      , bench "ifolded"         $ nf (itoListOf ifolded) h
      , bench "ifolded/lens"    $ nf (L.itoListOf L.ifolded) h
      ]
    , bgroup "sum"
      [ bench "native"          $ nf (sum . F.toList) h
      , bench "each"            $ nf (sumOf each) h
      , bench "each/lens"       $ nf (L.sumOf L.each) h
      , bench "itraversed"      $ nf (sumOf itraversed) h
      , bench "itraversed/lens" $ nf (L.sumOf L.itraversed) h
      , bench "ifolded"         $ nf (sumOf ifolded) h
      , bench "ifolded/lens"    $ nf (L.sumOf L.ifolded) h
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
