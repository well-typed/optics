module Optics.ReadOnly
  ( ToReadOnly(..)
  ) where

import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Class for read-write optics that have their read-only counterparts.
class ToReadOnly k s t a b where
  -- | Turn read-write optic into its read-only counterpart (or leave read-only
  -- optics as-is).
  --
  -- This is useful when you have an @optic :: Optic k is s t a b@ of read-write
  -- kind @k@ such that @s@, @t@, @a@, @b@ are rigid, there is no evidence that
  -- @s ~ t@ and @a ~ b@ and you want to pass @optic@ to one of the functions
  -- that accept read-only optic kinds.
  --
  -- Example:
  --
  -- @
  -- λ> let fstIntToChar = _1 :: Lens (Int, r) (Char, r) Int Char
  -- λ> :t view fstIntToChar
  --
  -- <interactive>:1:6: error:
  --     • Couldn't match type ‘Char’ with ‘Int’
  --       Expected type: Optic' A_Lens NoIx (Int, r) Int
  --         Actual type: Lens (Int, r) (Char, r) Int Char
  --     • In the first argument of ‘view’, namely ‘fstIntToChar’
  --       In the expression: view fstIntToChar
  -- λ> :t view (getting fstIntToChar)
  -- view (getting fstIntToChar) :: (Int, r) -> Int
  -- @
  getting :: Optic k is s t a b -> Optic' (Join A_Getter k) is s a

instance ToReadOnly An_Iso s t a b where
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Lens s t a b where
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Prism s t a b where
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly An_AffineTraversal s t a b where
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_Traversal s t a b where
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance ToReadOnly A_PrismaticGetter s t a b where
  getting o = Optic (getting__ o)
  {-# INLINE getting #-}

instance (s ~ t, a ~ b) => ToReadOnly A_Getter s t a b where
  getting = id
  {-# INLINE getting #-}

instance (s ~ t, a ~ b) => ToReadOnly An_AffineFold s t a b where
  getting = id
  {-# INLINE getting #-}

instance (s ~ t, a ~ b) => ToReadOnly A_Fold s t a b where
  getting = id
  {-# INLINE getting #-}

-- | Internal implementation of 'getting'.
getting__
  :: (Profunctor p, Bicontravariant p, Constraints k p)
  => Optic k            is    s t a b
  -> Optic__ p i (Curry is i) s s a a
getting__ (Optic o) = rphantom . o . rphantom
{-# INLINE getting__ #-}
