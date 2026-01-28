{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Optics.Tests.Utils where

import Language.Haskell.TH (Name)
import Test.Tasty.HUnit
import Test.Inspection
import qualified GHC.Generics as G

import Optics.Internal.Optic
import qualified Data.Profunctor.Indexed as P

hasNoProfunctors :: Name -> Obligation
hasNoProfunctors name = mkObligation name $ NoUseOf
  [ 'P.dimap
  , 'P.lmap
  , 'P.rmap
  , 'P.lcoerce'
  , 'P.rcoerce'
  , 'P.conjoined__
  , 'P.ixcontramap
  , 'P.first'
  , 'P.second'
  , 'P.linear
  , 'P.ilinear
  , 'P.unfirst
  , 'P.unsecond
  , 'P.left'
  , 'P.right'
  , 'P.unleft
  , 'P.unright
  , 'P.visit
  , 'P.ivisit
  , 'P.wander
  , 'P.iwander
  , 'P.roam
  , 'P.iroam
  , 'appendIndices
  , 'composeN
  ]

hasNoIndexClasses :: Name -> Obligation
hasNoIndexClasses name = mkObligation name $ NoUseOf
  [ 'appendIndices
  , 'composeN
  ]

-- | 'hasNoGenerics' from 'Test.Inspection' checks for lack of data types, but
-- they show up in coercions even though the representation was optimized away;
-- check for functions and data constructors instead.
hasNoGenericRep :: Name -> Obligation
hasNoGenericRep name = mkObligation name $ NoUseOf
  [ 'G.from
  , 'G.to
  , '(G.:*:)
  , 'G.K1
  , 'G.L1
  , 'G.M1
  , 'G.R1
  , 'G.U1
  ]

assertSuccess :: Result -> IO ()
assertSuccess (Success _)   = return ()
assertSuccess (Failure err) = assertFailure err

assertFailure' :: Result -> IO ()
assertFailure' (Success err) = assertFailure err
assertFailure' (Failure _)   = return ()

ghc82to86failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 802 && __GLASGOW_HASKELL__ <= 806
ghc82to86failure = assertFailure'
#else
ghc82to86failure = assertSuccess
#endif

ghc86to810failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 810
ghc86to810failure = assertFailure'
#else
ghc86to810failure = assertSuccess
#endif

ghc82failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 802
ghc82failure = assertFailure'
#else
ghc82failure = assertSuccess
#endif

ghc810failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 810
ghc810failure = assertFailure'
#else
ghc810failure = assertSuccess
#endif

ghcGE86failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 806
ghcGE86failure = assertFailure'
#else
ghcGE86failure = assertSuccess
#endif

ghcLE84failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ <= 804
ghcLE84failure = assertFailure'
#else
ghcLE84failure = assertSuccess
#endif

ghc82andGE90failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 802 \
 || __GLASGOW_HASKELL__ >= 900
ghc82andGE90failure = assertFailure'
#else
ghc82andGE90failure = assertSuccess
#endif

ghc82and86to810and92to94failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 802 \
 || __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 810 \
 || __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ <= 904
ghc82and86to810and92to94failure = assertFailure'
#else
ghc82and86to810and92to94failure = assertSuccess
#endif

ghcGE90failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 900
ghcGE90failure = assertFailure'
#else
ghcGE90failure = assertSuccess
#endif

ghc92and94failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ <= 904
ghc92and94failure = assertFailure'
#else
ghc92and94failure = assertSuccess
#endif

ghc86to810and92to94failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 810 \
 || __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ <= 904
ghc86to810and92to94failure = assertFailure'
#else
ghc86to810and92to94failure = assertSuccess
#endif

ghc86to910failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 910
ghc86to910failure = assertFailure'
#else
ghc86to910failure = assertSuccess
#endif

ghc912failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 912 && __GLASGOW_HASKELL__ <= 912
ghc912failure = assertFailure'
#else
ghc912failure = assertSuccess
#endif

ghcGE912failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 912
ghcGE912failure = assertFailure'
#else
ghcGE912failure = assertSuccess
#endif

ghc810andGE912failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 810 \
  || __GLASGOW_HASKELL__ >= 912
ghc810andGE912failure = assertFailure'
#else
ghc810andGE912failure = assertSuccess
#endif

-- GHC-9.12.2 has issues with nospec being in the way.
-- TODO: Later we should probably forbid GHC-9.12.2 usage.
-- Until GHC-9.12.3 (with fix) is released it would be too prohibitive though.
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0) && !MIN_VERSION_GLASGOW_HASKELL(9,12,3,0)
#define GHC_9122 1
#else
#define GHC_9122 0
#endif

ghc9122failure :: Result -> IO ()
#if GHC_9122
ghc9122failure = assertFailure'
#else
ghc9122failure = assertSuccess
#endif

ghc86to910and9122failure :: Result -> IO ()
#if (__GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 910) || GHC_9122
ghc86to910and9122failure = assertFailure'
#else
ghc86to910and9122failure = assertSuccess
#endif

ghc92and94and9122failure :: Result -> IO ()
#if (__GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ <= 904) || GHC_9122
ghc92and94and9122failure = assertFailure'
#else
ghc92and94and9122failure = assertSuccess
#endif

ghc82and9122failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 802 || GHC_9122
ghc82and9122failure = assertFailure'
#else
ghc82and9122failure = assertSuccess
#endif

ghcLE84and9122failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ <= 804 || GHC_9122
ghcLE84and9122failure = assertFailure'
#else
ghcLE84and9122failure = assertSuccess
#endif
