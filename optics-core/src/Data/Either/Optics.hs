-- | Module: Data.Either.Optics
-- Description: 'Prism's for the 'Either' datatype.
--
-- This module defines 'Prism's for the constructors of the 'Either' datatype.
module Data.Either.Optics
  ( _Left
  , _Right
  )
  where

import Optics.Prism

-- | A 'Prism' that matches on the 'Left' constructor of 'Either'.
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

-- | A 'Prism' that matches on the 'Right' constructor of 'Either'.
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
