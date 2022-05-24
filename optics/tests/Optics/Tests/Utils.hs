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

ghc86to810and92to94failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ <= 810 \
 || __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ <= 904
ghc86to810and92to94failure = assertFailure'
#else
ghc86to810and92to94failure = assertSuccess
#endif
