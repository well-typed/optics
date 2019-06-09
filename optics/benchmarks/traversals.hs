{-# LANGUAGE CPP #-}
module Main where

import Criterion.Main
import Criterion.Types
import Data.Char
import qualified Control.Lens as L
import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lens as L
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
      , bench "each/lens" $
          nf (\x -> S.execState (L.traverseOf L.each (S.modify' . (+)) x) 0) v
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) v
      , bench "itraversed/lens" $
          nf (\x -> S.execState (L.traverseOf L.itraversed (S.modify' . (+)) x) 0) v
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (traverse (\(i, a) -> S.modify' $ (i + a +)) $ V.indexed x) 0) v
      , bench "itraverse" $ nf (\x -> S.execState (itraverse (\i a -> S.modify' $ (i + a +)) x) 0) v
      , bench "itraverse/lens" $ nf (\x -> S.execState (L.itraverse (\i a -> S.modify' $ (i + a +)) x) 0) v
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) v
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) v
      , bench "itraversed/lens" $ nf (\x -> S.execState (L.itraverseOf L.itraversed (\i a -> S.modify' $ (i + a +)) x) 0) v
      ]
    , bgroup "map"
      [ bench "native"          $ nf (V.map               (+100)) v
      , bench "each"            $ nf (over each           (+100)) v
      , bench "each/lens"       $ nf (L.over L.each       (+100)) v
      , bench "itraversed"      $ nf (over itraversed     (+100)) v
      , bench "itraversed/lens" $ nf (L.over L.itraversed (+100)) v
      , bench "imapped"         $ nf (over imapped        (+100)) v
      , bench "imapped/lens"    $ nf (L.over L.imapped    (+100)) v
      ]
    , bgroup "imap"
      [ bench "native"          $ nf (V.imap               (\i x -> x + i +100)) v
      , bench "imap"            $ nf (imap                 (\i x -> x + i +100)) v
      , bench "imap/lens"       $ nf (L.imap               (\i x -> x + i +100)) v
      , bench "each"            $ nf (iover each           (\i x -> x + i +100)) v
      , bench "itraversed"      $ nf (iover itraversed     (\i x -> x + i +100)) v
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (\i x -> x + i +100)) v
      , bench "imapped"         $ nf (iover imapped        (\i x -> x + i +100)) v
      , bench "imapped/lens"    $ nf (L.iover L.imapped    (\i x -> x + i +100)) v
      ]
    , bgroup "elements"
      [ bench "itraversed"      $ nf (iover itraversed (+)) v
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (+)) v
      , bench "elements"        $ nf (iover (elements $ const True) (+)) v
      , bench "elements/lens"   $ nf (L.iover (L.elements $ const True) (+)) v
      ]
    , bgroup "partsOf"
      [ bench "partsOf"        $ nf (over (partsOf traversed) reverse) v
      , bench "partsOf/lens"   $ nf (L.over (L.partsOf traverse) reverse) v
      , bench "ipartsOf"       $ nf (iover (ipartsOf itraversed) (\is -> reverse . zipWith (+) is)) v
      , bench "ipartsOf/lens"  $ nf (L.iover (L.ipartsOf L.itraversed) (\is -> reverse . zipWith (+) is)) v
      ]
    , bgroup "indices"
      [ bench "indices"         $ nf (iover (itraversed %& indices even) (+)) v
      , bench "indices/lens"    $ nf (L.iover (L.itraversed . L.indices even) (+)) v
      ]
    ]
  , bgroup "unboxed-vector"
    [ bgroup "map"
      [ bench "native"          $ nf (U.map (+100)) u
      , bench "each"            $ nf (over each (+100)) u
      , bench "each/lens"       $ nf (L.over L.each (+100)) u
      ]
    , bgroup "imap"
      [ bench "native"          $ nf (U.imap (\i x -> x + i +100)) u
      , bench "each"            $ nf (iover each (\i x -> x + i)) u
      ]
    ]
  , bgroup "sequence"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) s
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) s
      , bench "each/lens" $
          nf (\x -> S.execState (L.traverseOf L.each (S.modify' . (+)) x) 0) s
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) s
      , bench "itraversed/lens" $
          nf (\x -> S.execState (L.traverseOf L.itraversed (S.modify' . (+)) x) 0) s
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (seqTraverseWithIndex (\i a -> S.modify' $ (i + a +)) x) 0) s
      , bench "itraverse " $ nf (\x -> S.execState (itraverse (\i a -> S.modify' $ (i + a +)) x) 0) s
      , bench "itraverse/lens" $ nf (\x -> S.execState (L.itraverse (\i a -> S.modify' $ (i + a +)) x) 0) s
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) s
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) s
      , bench "itraversed/lens" $ nf (\x -> S.execState (L.itraverseOf L.itraversed (\i a -> S.modify' $ (i + a +)) x) 0) s
      ]
    , bgroup "map"
      [ bench "native"          $ nf (fmap                (+100)) s
      , bench "each"            $ nf (over each           (+100)) s
      , bench "each/lens"       $ nf (L.over L.each       (+100)) s
      , bench "itraversed"      $ nf (over itraversed     (+100)) s
      , bench "itraversed/lens" $ nf (L.over L.itraversed (+100)) s
      , bench "imapped"         $ nf (over imapped        (+100)) s
      , bench "imapped/lens"    $ nf (L.over L.imapped    (+100)) s
      ]
    , bgroup "imap"
      [ bench "native"          $ nf (S.mapWithIndex       (\i x -> x + i +100)) s
      , bench "imap"            $ nf (imap                 (\i x -> x + i +100)) s
      , bench "imap/lens"       $ nf (L.imap               (\i x -> x + i +100)) s
      , bench "each"            $ nf (iover each           (\i x -> x + i +100)) s
      , bench "itraversed"      $ nf (iover itraversed     (\i x -> x + i +100)) s
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (\i x -> x + i +100)) s
      , bench "imapped"         $ nf (iover imapped        (\i x -> x + i +100)) s
      , bench "imapped/lens"    $ nf (L.iover L.imapped    (\i x -> x + i +100)) s
      ]
    , bgroup "elements"
      [ bench "itraversed"      $ nf (iover itraversed (+)) s
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (+)) s
      , bench "elements"        $ nf (iover (elements $ const True) (+)) s
      , bench "elements/lens"   $ nf (L.iover (L.elements $ const True) (+)) s
      ]
    , bgroup "partsOf"
      [ bench "partsOf"        $ nf (over (partsOf traversed) reverse) s
      , bench "partsOf/lens"   $ nf (L.over (L.partsOf traverse) reverse) s
      , bench "ipartsOf"       $ nf (iover (ipartsOf itraversed) (\is -> reverse . zipWith (+) is)) s
      , bench "ipartsOf/lens"  $ nf (L.iover (L.ipartsOf L.itraversed) (\is -> reverse . zipWith (+) is)) s
      ]
    , bgroup "indices"
      [ bench "indices"         $ nf (iover (itraversed %& indices even) (+)) s
      , bench "indices/lens"    $ nf (L.iover (L.itraversed . L.indices even) (+)) s
      ]
    ]
  , bgroup "bytestring"
    [ bgroup "map"
      [ bench "native"    $ nf (BS.map         (+100)) b
      , bench "each"      $ nf (over     each  (+100)) b
      , bench "each/lens" $ nf (L.over L.each  (+100)) b
      ]
    , bgroup "imap"
      [ bench "bytes"      $ nf (iover     bytes (\i x -> x + fromIntegral i)) b
      , bench "bytes/lens" $ nf (L.iover L.bytes (\i x -> x + fromIntegral i)) b
      ]
    ]
  , bgroup "bytestring char8"
    [ bgroup "map"
      [ bench "native"     $ nf (BS8.map        (chr . (+100) . ord)) b
      , bench "chars"      $ nf (over chars     (chr . (+100) . ord)) b
      , bench "chars/lens" $ nf (L.over L.chars (chr . (+100) . ord)) b
      ]
    , bgroup "imap"
      [ bench "chars" $ nf
          (iover chars (\i x -> chr $ ord x + fromIntegral (i `mod` 256))) b
      , bench "chars/lens" $ nf
          (L.iover L.chars (\i x -> chr $ ord x + fromIntegral (i `mod` 256))) b
      ]
    ]
  , bgroup "bytestring lazy"
    [ bgroup "map"
      [ bench "native"    $ nf (BSL.map        (+100)) bl
      , bench "each"      $ nf (over     each  (+100)) bl
      , bench "each/lens" $ nf (L.over L.each  (+100)) bl
      ]
    , bgroup "imap"
      [ bench "bytes"      $ nf (iover     bytes (\i x -> x + fromIntegral i)) bl
      , bench "bytes/lens" $ nf (L.iover L.bytes (\i x -> x + fromIntegral i)) bl
      ]
    ]
  , bgroup "bytestring lazy char8"
    [ bgroup "map"
      [ bench "native"     $ nf (BSL8.map        (chr . (+100) . ord)) bl
      , bench "chars"      $ nf (over     chars  (chr . (+100) . ord)) bl
      , bench "chars/lens" $ nf (L.over L.chars  (chr . (+100) . ord)) bl
      ]
    , bgroup "imap"
      [ bench "chars" $ nf
          (iover chars (\i x -> chr $ ord x + fromIntegral (i `mod` 256))) bl
      , bench "chars/lens" $ nf
          (L.iover L.chars (\i x -> chr $ ord x + fromIntegral (i `mod` 256))) bl
      ]
    ]
  , bgroup "list"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) l
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) l
      , bench "each/lens" $
          nf (\x -> S.execState (L.traverseOf L.each (S.modify' . (+)) x) 0) l
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) l
      , bench "itraversed/lens" $
          nf (\x -> S.execState (L.traverseOf L.itraversed (S.modify' . (+)) x) 0) l
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (traverse (\(i, a) -> S.modify' $ (i + a +)) (zip [0..] x)) 0) l
      , bench "itraverse" $ nf (\x -> S.execState (itraverse (\i a -> S.modify' $ (i + a +)) x) 0) l
      , bench "itraverse/lens" $ nf (\x -> S.execState (L.itraverse (\i a -> S.modify' $ (i + a +)) x) 0) l
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) l
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) l
      , bench "itraversed/lens" $ nf (\x -> S.execState (L.itraverseOf L.itraversed (\i a -> S.modify' $ (i + a +)) x) 0) l
      ]
    , bgroup "map"
      [ bench "native"          $ nf (map                 (+100)) l
      , bench "each"            $ nf (over each           (+100)) l
      , bench "each/lens"       $ nf (L.over L.each       (+100)) l
      , bench "itraversed"      $ nf (over itraversed     (+100)) l
      , bench "itraversed/lens" $ nf (L.over L.itraversed (+100)) l
      , bench "imapped"         $ nf (over imapped        (+100)) l
      , bench "imapped/lens"    $ nf (L.over L.imapped    (+100)) l
      ]
    , bgroup "imap"
      [ bench "imap"            $ nf (imap   (\i x -> x + i +100)) l
      , bench "imap/lens"       $ nf (L.imap (\i x -> x + i +100)) l
      , bench "each"            $ nf (iover each (\i x -> x + i +100)) l
      , bench "itraversed"      $ nf (iover itraversed (\i x -> x + i +100)) l
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (\i x -> x + i +100)) l
      , bench "imapped"         $ nf (iover imapped (\i x -> x + i +100)) l
      , bench "imapped/lens"    $ nf (L.iover L.imapped (\i x -> x + i +100)) l
      ]
    , bgroup "elements"
      [ bench "itraversed"      $ nf (iover itraversed (+)) l
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (+)) l
      , bench "elements"        $ nf (iover (elements $ const True) (+)) l
      , bench "elements/lens"   $ nf (L.iover (L.elements $ const True) (+)) l
      ]
    , bgroup "partsOf"
      [ bench "partsOf"        $ nf (over (partsOf traversed) reverse) l
      , bench "partsOf/lens"   $ nf (L.over (L.partsOf traverse) reverse) l
      , bench "ipartsOf"       $ nf (iover (ipartsOf itraversed) (\is -> reverse . zipWith (+) is)) l
      , bench "ipartsOf/lens"  $ nf (L.iover (L.ipartsOf L.itraversed) (\is -> reverse . zipWith (+) is)) l
      ]
    , bgroup "indices"
      [ bench "indices"         $ nf (iover (itraversed %& indices even) (+)) l
      , bench "indices/lens"    $ nf (L.iover (L.itraversed . L.indices even) (+)) l
      ]
    ]
  , bgroup "intmap"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) im
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) im
      , bench "each/lens" $
          nf (\x -> S.execState (L.traverseOf L.each (S.modify' . (+)) x) 0) im
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) im
      , bench "itraversed/lens" $
          nf (\x -> S.execState (L.traverseOf L.itraversed (S.modify' . (+)) x) 0) im
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (IM.traverseWithKey (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "itraverse" $ nf (\x -> S.execState (itraverse (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "itraverse/lens" $ nf (\x -> S.execState (L.itraverse (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) im
      , bench "itraversed/lens" $ nf (\x -> S.execState (L.itraverseOf L.itraversed (\i a -> S.modify' $ (i + a +)) x) 0) im
      ]
    , bgroup "map"
      [ bench "native"          $ nf (fmap                (+100)) im
      , bench "each"            $ nf (over each           (+100)) im
      , bench "each/lens"       $ nf (L.over L.each       (+100)) im
      , bench "itraversed"      $ nf (over itraversed     (+100)) im
      , bench "itraversed/lens" $ nf (L.over L.itraversed (+100)) im
      , bench "imapped"         $ nf (over imapped       (+100)) im
      , bench "imapped/lens"    $ nf (L.over L.imapped    (+100)) im
      ]
    , bgroup "imap"
      [ bench "native"          $ nf (IM.mapWithKey        (\i x -> x + i +100)) im
      , bench "imap"            $ nf (imap                 (\i x -> x + i +100)) im
      , bench "imap/lens"       $ nf (L.imap               (\i x -> x + i +100)) im
      , bench "each"            $ nf (iover each           (\i x -> x + i +100)) im
      , bench "itraversed"      $ nf (iover itraversed     (\i x -> x + i +100)) im
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (\i x -> x + i +100)) im
      , bench "imapped"         $ nf (iover imapped        (\i x -> x + i +100)) im
      , bench "imapped/lens"    $ nf (L.iover L.imapped    (\i x -> x + i +100)) im
      ]
    , bgroup "elements"
      [ bench "itraversed"      $ nf (iover itraversed (+)) im
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (+)) im
      , bench "elements"        $ nf (iover (elements $ const True) (+)) im
      , bench "elements/lens"   $ nf (L.iover (L.elements $ const True) (+)) im
      ]
    , bgroup "partsOf"
      [ bench "partsOf"        $ nf (over (partsOf traversed) reverse) im
      , bench "partsOf/lens"   $ nf (L.over (L.partsOf traverse) reverse) im
      , bench "ipartsOf"       $ nf (iover (ipartsOf itraversed) (\is -> reverse . zipWith (+) is)) im
      , bench "ipartsOf/lens"  $ nf (L.iover (L.ipartsOf L.itraversed) (\is -> reverse . zipWith (+) is)) im
      ]
    , bgroup "indices"
      [ bench "indices"         $ nf (iover (itraversed %& indices even) (+)) im
      , bench "indices/lens"    $ nf (L.iover (L.itraversed . L.indices even) (+)) im
      ]
    ]
  , bgroup "map"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) m
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) m
      , bench "each/lens" $
          nf (\x -> S.execState (L.traverseOf L.each (S.modify' . (+)) x) 0) m
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) m
      , bench "itraversed/lens" $
          nf (\x -> S.execState (L.traverseOf L.itraversed (S.modify' . (+)) x) 0) m
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (M.traverseWithKey (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "itraverse" $ nf (\x -> S.execState (itraverse (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "itraverse/lens" $ nf (\x -> S.execState (L.itraverse (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) m
      , bench "itraversed/lens" $ nf (\x -> S.execState (L.itraverseOf L.itraversed (\i a -> S.modify' $ (i + a +)) x) 0) m
      ]
    , bgroup "map"
      [ bench "native"          $ nf (fmap                (+100)) m
      , bench "each"            $ nf (over each           (+100)) m
      , bench "each/lens"       $ nf (L.over L.each       (+100)) m
      , bench "itraversed"      $ nf (over itraversed     (+100)) m
      , bench "itraversed/lens" $ nf (L.over L.itraversed (+100)) m
      , bench "imapped"         $ nf (over imapped        (+100)) m
      , bench "imapped/lens"    $ nf (L.over L.imapped    (+100)) m
      ]
    , bgroup "imap"
      [ bench "native"          $ nf (M.mapWithKey         (\i x -> x + i +100)) m
      , bench "imap"            $ nf (imap                 (\i x -> x + i +100)) m
      , bench "imap/lens"       $ nf (L.imap               (\i x -> x + i +100)) m
      , bench "each"            $ nf (iover each           (\i x -> x + i +100)) m
      , bench "itraversed"      $ nf (iover itraversed     (\i x -> x + i +100)) m
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (\i x -> x + i +100)) m
      , bench "imapped"         $ nf (iover imapped        (\i x -> x + i +100)) m
      , bench "imapped/lens"    $ nf (L.iover L.imapped    (\i x -> x + i +100)) m
      ]
    , bgroup "elements"
      [ bench "itraversed"      $ nf (iover itraversed (+)) m
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (+)) m
      , bench "elements"        $ nf (iover (elements $ const True) (+)) m
      , bench "elements/lens"   $ nf (L.iover (L.elements $ const True) (+)) m
      ]
    , bgroup "partsOf"
      [ bench "partsOf"        $ nf (over (partsOf traversed) reverse) m
      , bench "partsOf/lens"   $ nf (L.over (L.partsOf traverse) reverse) m
      , bench "ipartsOf"       $ nf (iover (ipartsOf itraversed) (\is -> reverse . zipWith (+) is)) m
      , bench "ipartsOf/lens"  $ nf (L.iover (L.ipartsOf L.itraversed) (\is -> reverse . zipWith (+) is)) m
      ]
    , bgroup "indices"
      [ bench "indices"         $ nf (iover (itraversed %& indices even) (+)) m
      , bench "indices/lens"    $ nf (L.iover (L.itraversed . L.indices even) (+)) m
      ]
    ]
  , bgroup "hash map"
    [ bgroup "traverse"
      [ bench "native" $
          nf (\x -> S.execState (traverse (S.modify' . (+)) x) 0) h
      , bench "each" $
          nf (\x -> S.execState (traverseOf each (S.modify' . (+)) x) 0) h
      , bench "each/lens" $
          nf (\x -> S.execState (L.traverseOf L.each (S.modify' . (+)) x) 0) h
      , bench "itraversed" $
          nf (\x -> S.execState (traverseOf itraversed (S.modify' . (+)) x) 0) h
      , bench "itraversed/lens" $
          nf (\x -> S.execState (L.traverseOf L.itraversed (S.modify' . (+)) x) 0) h
      ]
    , bgroup "itraverse"
      [ bench "native" $ nf (\x -> S.execState (HM.traverseWithKey (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "itraverse" $ nf (\x -> S.execState (itraverse (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "itraverse/lens" $ nf (\x -> S.execState (L.itraverse (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "each" $ nf (\x -> S.execState (itraverseOf each (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "itraversed" $ nf (\x -> S.execState (itraverseOf itraversed (\i a -> S.modify' $ (i + a +)) x) 0) h
      , bench "itraversed/lens" $ nf (\x -> S.execState (L.itraverseOf L.itraversed (\i a -> S.modify' $ (i + a +)) x) 0) h
      ]
    , bgroup "map"
      [ bench "native"          $ nf (HM.map              (+100)) h
      , bench "each"            $ nf (over each           (+100)) h
      , bench "each/lens"       $ nf (L.over L.each       (+100)) h
      , bench "itraversed"      $ nf (over itraversed     (+100)) h
      , bench "itraversed/lens" $ nf (L.over L.itraversed (+100)) h
      , bench "imapped"         $ nf (over imapped        (+100)) h
      , bench "imapped/lens"    $ nf (L.over L.imapped    (+100)) h
      ]
    , bgroup "imap"
      [ bench "native"          $ nf (HM.mapWithKey        (\i x -> x + i +100)) h
      , bench "imap"            $ nf (imap                 (\i x -> x + i +100)) h
      , bench "imap/lens"       $ nf (L.imap               (\i x -> x + i +100)) h
      , bench "each"            $ nf (iover each           (\i x -> x + i +100)) h
      , bench "itraversed"      $ nf (iover itraversed     (\i x -> x + i +100)) h
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (\i x -> x + i +100)) h
      , bench "imapped"         $ nf (iover imapped        (\i x -> x + i +100)) h
      , bench "imapped/lens"    $ nf (L.iover L.imapped    (\i x -> x + i +100)) h
      ]
    , bgroup "elements"
      [ bench "itraversed"      $ nf (iover itraversed (+)) h
      , bench "itraversed/lens" $ nf (L.iover L.itraversed (+)) h
      , bench "elements"        $ nf (iover (elements $ const True) (+)) h
      , bench "elements/lens"   $ nf (L.iover (L.elements $ const True) (+)) h
      ]
    , bgroup "partsOf"
      [ bench "partsOf"        $ nf (over (partsOf traversed) reverse) h
      , bench "partsOf/lens"   $ nf (L.over (L.partsOf traverse) reverse) h
      , bench "ipartsOf"       $ nf (iover (ipartsOf itraversed) (\is -> reverse . zipWith (+) is)) h
      , bench "ipartsOf/lens"  $ nf (L.iover (L.ipartsOf L.itraversed) (\is -> reverse . zipWith (+) is)) h
      ]
    , bgroup "indices"
      [ bench "indices"         $ nf (iover (itraversed %& indices even) (+)) h
      , bench "indices/lens"    $ nf (L.iover (L.itraversed . L.indices even) (+)) h
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
