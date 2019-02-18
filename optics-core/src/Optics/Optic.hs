{-# LANGUAGE CPP #-}
module Optics.Optic
  ( Optic
  , Optic'
  , NoIx
  , WithIx
  , Is
  , castOptic
  , Join
  , (%)
  , (%%)
  -- * Labels
  , LabelOptic(..)
  , LabelOptic'
  -- * Misc
  , Append
  , NonEmptyIndices
  , HasSingleIndex
    -- * Base re-exports
  , (&)
  , (<&>)
  )
  where

import Data.Function

import Optics.Internal.Indexed
import Optics.Internal.Optic

#if MIN_VERSION_base(4,11,0)
import Data.Functor ((<&>))
#else
-- | Infix flipped 'fmap'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
#endif
