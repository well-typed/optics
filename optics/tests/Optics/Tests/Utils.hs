{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Optics.Tests.Utils where

import Language.Haskell.TH (Name)
import Test.Tasty.HUnit
import Test.Inspection

import qualified Optics.Internal.Profunctor as P

hasNoProfunctors :: Name -> Obligation
hasNoProfunctors name = mkObligation name $ NoUseOf
  [ 'P.dimap
  , 'P.lmap
  , 'P.rmap
  , 'P.lcoerce'
  , 'P.rcoerce'
  , 'P.first'
  , 'P.second'
  , 'P.linear
  , 'P.unfirst
  , 'P.unsecond
  , 'P.left'
  , 'P.right'
  , 'P.unleft
  , 'P.unright
  , 'P.visit
  , 'P.ivisit
  , 'P.conjoined'
  , 'P.ixcontramap
  , 'P.wander
  , 'P.iwander
  , 'P.roam
  , 'P.iroam
  ]

assertSuccess :: Result -> IO ()
assertSuccess (Success _)   = return ()
assertSuccess (Failure err) = assertFailure err

assertFailure' :: Result -> IO ()
assertFailure' (Success err) = assertFailure err
assertFailure' (Failure _)   = return ()

ghc80failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 800
ghc80failure = assertFailure'
#else
ghc80failure = assertSuccess
#endif

ghc82failure :: Result -> IO ()
#if __GLASGOW_HASKELL__ == 802
ghc82failure = assertFailure'
#else
ghc82failure = assertSuccess
#endif
