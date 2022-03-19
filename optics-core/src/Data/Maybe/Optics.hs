-- |
-- Module: Data.Maybe.Optics
-- Description: 'Prism's for the 'Maybe' datatype.
--
-- This module defines 'Prism's for the constructors of the 'Maybe' datatype.
module Data.Maybe.Optics
  ( _Nothing
  , _Just
  , (%?)
  )
  where

import Optics.Internal.Optic
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

-- | Shortcut for @'%' '_Just' '%'@.
--
-- Useful for composing lenses of 'Maybe' type.
--
-- @since 0.4.1
infixl 9 %?
(%?)
  :: (AppendIndices is js ks, JoinKinds k A_Prism k', JoinKinds k' l m)
  => Optic k is s t (Maybe u) (Maybe v)
  -> Optic l js u v a b
  -> Optic m ks s t a b
o1 %? o2 = o1 % _Just % o2
{-# INLINE (%?) #-}
