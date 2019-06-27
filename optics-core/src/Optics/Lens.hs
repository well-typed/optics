-- |
-- Module: Optics.Lens
-- Description: A generalised or first-class field.
--
-- A 'Lens' is a generalised or first-class field.
--
-- If we have a value @s :: S@, and a @l :: 'Lens'' S A@, we can /get/
-- the "field value" of type @A@ using @'Optics.Getter.view' l s@.  We
-- can also /update/ (or /put/ or /set/) the value using
-- 'Optics.Setter.over' (or 'Optics.Setter.set').
--
-- For example, given the following definitions:
--
-- >>> data Human = Human { _name :: String, _location :: String } deriving Show
-- >>> let human = Human "Bob" "London"
--
-- we can make a 'Lens' for @_name@ field:
--
-- >>> let name = lens _name $ \s x -> s { _name = x }
--
-- which we can use as a 'Optics.Getter.Getter':
--
-- >>> view name human
-- "Bob"
--
-- or a 'Optics.Setter.Setter':
--
-- >>> set name "Robert" human
-- Human {_name = "Robert", _location = "London"}
--
module Optics.Lens
  (
  -- * Formation
    Lens
  , Lens'

  -- * Introduction
  , lens

  -- * Elimination
  -- | A 'Lens' is a 'Optics.Setter.Setter' and a
  -- 'Optics.Getter.Getter', therefore you can specialise types to
  -- obtain:
  --
  -- @
  -- 'Optics.Getter.view' :: 'Lens' i s t a b -> s -> a
  -- 'Optics.Setter.set'  :: 'Lens' i s t a b -> b -> s -> t
  -- @
  --

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Getter.view' ('lens' f g)   s ≡ f s
  -- 'Optics.Setter.set'  ('lens' f g) a s ≡ g s a
  -- @

  -- * Well-formedness
  -- |
  --
  -- * __GetPut__: You get back what you put in:
  --
  --     @
  --     'Optics.Getter.view' l ('Optics.Setter.set' l v s) ≡ v
  --     @
  --
  -- * __PutGet__: Putting back what you got doesn’t change anything:
  --
  --     @
  --     'Optics.Setter.set' l ('Optics.Getter.view' l s) s ≡ s
  --     @
  --
  -- * __PutPut__: Setting twice is the same as setting once:
  --
  --     @
  --     'Optics.Setter.set' l v' ('Optics.Setter.set' l v s) ≡ 'Optics.Setter.set' l v' s
  --     @
  --

  -- * Additional introduction forms
  -- | See "Data.Tuple.Optics" for 'Lens'es for tuples.
  , chosen
  , devoid
  , united

  -- * Additional elimination forms
  , withLens

  -- * Subtyping
  , A_Lens
  -- | <<Lens.png Lens in the optics hierarchy>>

  -- * van Laarhoven encoding
  -- | The van Laarhoven encoding of lenses is isomorphic to the profunctor
  -- encoding used internally by @optics@, but converting back and forth may
  -- have a performance penalty.
  , LensVL
  , LensVL'
  , lensVL
  , toLensVL
  , withLensVL

  -- * Re-exports
  , module Optics.Optic
  )
  where

import Data.Void

import Optics.Internal.Concrete
import Optics.Internal.Optic
import Optics.Internal.Profunctor
import Optics.Internal.Utils
import Optics.Optic

-- | Type synonym for a type-modifying lens.
type Lens s t a b = Optic A_Lens NoIx s t a b

-- | Type synonym for a type-preserving lens.
type Lens' s a = Optic' A_Lens NoIx s a

-- | Type synonym for a type-modifying van Laarhoven lens.
type LensVL s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven lens.
type LensVL' s a = LensVL s s a a

-- | Build a lens from a getter and a setter, which must respect the
-- well-formedness laws.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = Optic $
  -- Do not define lens in terms of lensVL, mixing profunctor-style definitions
  -- with VL style implementation can lead to subpar generated code,
  -- i.e. updating often gets and then sets as opposed to updating in place.
  dimap (\s -> (get s, s))
        (\(b, s) -> set s b)
  . first'
{-# INLINE lens #-}

-- | Work with a lens as a getter and a setter.
--
-- @
-- 'withLens' ('lens' f g) k ≡ k f g
-- @
withLens
  :: Is k A_Lens
  => Optic k is s t a b
  -> ((s -> a) -> (s -> b -> t) -> r)
  -> r
withLens o k = case getOptic (castOptic @A_Lens o) $ Store id (\_ -> id) of
  Store get set -> k get set
{-# INLINE withLens #-}

-- | Build a lens from the van Laarhoven representation.
lensVL :: LensVL s t a b -> Lens s t a b
lensVL l = Optic (linear l)
{-# INLINE lensVL #-}

-- | Convert a lens to the van Laarhoven representation.
toLensVL :: Is k A_Lens => Optic k is s t a b -> LensVL s t a b
toLensVL o = runStar #. getOptic (castOptic @A_Lens o) .# Star
{-# INLINE toLensVL #-}

-- | Work with a lens in the van Laarhoven representation.
withLensVL
  :: Is k A_Lens
  => Optic k is s t a b
  -> (LensVL s t a b -> r)
  -> r
withLensVL o k = k (toLensVL o)
{-# INLINE withLensVL #-}

----------------------------------------
-- Lenses

-- | Focus on both sides of an 'Either'.
chosen :: Lens (Either a a) (Either b b) a b
chosen = lensVL $ \f -> either (fmap Left . f) (fmap Right . f)
{-# INLINE chosen #-}

-- | There is a field for every type in the 'Void'.
--
-- >>> set (mapped % devoid) 1 []
-- []
--
-- >>> over (_Just % devoid) abs Nothing
-- Nothing
--
devoid :: Lens' Void a
devoid = lens absurd const
{-# INLINE devoid #-}

-- | We can always retrieve a @()@ from any type.
--
-- >>> view united "hello"
-- ()
--
-- >>> set united () "hello"
-- "hello"
united :: Lens' a ()
united  = lens (const ()) const
{-# INLINE united #-}

-- $setup
-- >>> import Optics.Core
