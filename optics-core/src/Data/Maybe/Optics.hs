-- |
-- Module: Data.Maybe.Optics
-- Description: 'Prism's for the 'Maybe' datatype.
--
-- This module defines 'Prism's for the constructors of the 'Maybe' datatype.
module Data.Maybe.Optics
  ( _Nothing
  , _Just
  )
  where

import Optics.Prism

-- | A 'Prism' that matches on the 'Nothing' constructor of 'Maybe'.
_Nothing :: Prism' (Maybe a) ()
_Nothing =
  prism
    (\ () -> Nothing)
    (\ x ->
      case x of
        Nothing -> Right ()
        Just y  -> Left (Just y)
    )
{-# INLINE _Nothing #-}

-- | A 'Prism' that matches on the 'Just' constructor of 'Maybe'.
_Just :: Prism (Maybe a) (Maybe b) a b
_Just =
  prism
    Just
    (\ x ->
      case x of
        Nothing -> Left Nothing
        Just y  -> Right y
    )
{-# INLINE _Just #-}
