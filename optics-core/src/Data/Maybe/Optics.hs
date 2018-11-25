module Data.Maybe.Optics
  ( _Nothing
  , _Just
  )
  where

import Optics.Prism

_Nothing :: Prism' i (Maybe a) ()
_Nothing =
  prism
    (\ () -> Nothing)
    (\ x ->
      case x of
        Nothing -> Right ()
        Just y  -> Left (Just y)
    )
{-# INLINE _Nothing #-}

_Just :: Prism i (Maybe a) (Maybe b) a b
_Just =
  prism
    Just
    (\ x ->
      case x of
        Nothing -> Left Nothing
        Just y  -> Right y
    )
{-# INLINE _Just #-}
