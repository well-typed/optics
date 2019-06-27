-- |
-- Module: Optics.Getter
-- Description: A function considered as an 'Optic'.
--
-- A 'Getter' is simply a function considered as an 'Optic'.
--
-- Given a function @f :: S -> A@, we can convert it into a
-- @'Getter' S A@ using 'to', and convert back to a function using 'view'.
--
-- This is typically useful not when you have functions/'Getter's
-- alone, but when you are composing multiple 'Optic's to produce a
-- 'Getter'.
--
module Optics.Getter
  (
  -- * Formation
   Getter

  -- * Introduction
  , to

  -- * Elimination
  , view
  , views

  -- * Computation
  -- |
  --
  -- @
  -- 'view' ('to' f) ≡ f
  -- @

  -- * Well-formedness
  -- | A 'Getter' is not subject to any laws.

  -- * Subtyping
  , A_Getter
  -- | <<Getter.png Getter in the optics hierarchy>>

  -- * Re-exports
  , module Optics.Optic
  )
  where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Optic

-- | Type synonym for a getter.
type Getter s a = Optic' A_Getter NoIx s a

-- | View the value pointed to by a getter.
view :: Is k A_Getter => Optic' k is s a -> s -> a
view o = views o id
{-# INLINE view #-}

-- | View the function of the value pointed to by a getter.
views :: Is k A_Getter => Optic' k is s a -> (a -> r) -> s -> r
views o = \f -> runForget $ getOptic (castOptic @A_Getter o) (Forget f)
{-# INLINE views #-}

-- | Build a getter from a function.
to :: (s -> a) -> Getter s a
to f = Optic (lmap f . rphantom)
{-# INLINE to #-}
