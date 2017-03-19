module Data.Either.Optics
  ( _Left
  , _Right
  )
  where

import Optics

_Left :: Prism (Either a b) (Either c b) a c
_Left =
  prism
    Left
    (\ x ->
      case x of
        Left y  -> Right y
        Right y -> Left (Right y)
    )
{-# INLINE _Left #-}

_Right :: Prism (Either a b) (Either a c) b c
_Right =
  prism
    Right
    (\ x ->
      case x of
        Left y  -> Left (Left y)
        Right y -> Right y
    )
{-# INLINE _Right #-}
