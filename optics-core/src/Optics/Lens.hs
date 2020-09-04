{-# LANGUAGE QuantifiedConstraints #-}
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
  -- | A 'Lens' is in particular a 'Optics.Getter.Getter' and a
  -- 'Optics.Setter.Setter', therefore you can specialise types to obtain:
  --
  -- @
  -- 'Optics.Getter.view' :: 'Lens'' s a -> s -> a
  -- @
  --
  -- @
  -- 'Optics.Setter.over' :: 'Lens' s t a b -> (a -> b) -> s -> t
  -- 'Optics.Setter.set'  :: 'Lens' s t a b ->       b  -> s -> t
  -- @
  --
  -- If you want to 'Optics.Getter.view' a type-modifying 'Lens' that is
  -- insufficiently polymorphic to be used as a type-preserving 'Lens'', use
  -- 'Optics.ReadOnly.getting':
  --
  -- @
  -- 'Optics.Getter.view' . 'Optics.ReadOnly.getting' :: 'Lens' s t a b -> s -> a
  -- @

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
  --
  -- If you're looking for 'Optics.IxLens.chosen', it was moved to "Optics.IxLens".
  , equality'
  , alongside
  , united

  -- * Additional elimination forms
  , withLens

  -- * Subtyping
  , A_Lens
  -- | <<diagrams/Lens.png Lens in the optics hierarchy>>

  -- * van Laarhoven encoding
  -- | The van Laarhoven encoding of lenses is isomorphic to the profunctor
  -- encoding used internally by @optics@, but converting back and forth may
  -- have a performance penalty.
  , LensVL
  , LensVL'
  , lensVL
  , toLensVL
  , withLensVL
  )
  where

import Data.Profunctor.Indexed

import Optics.Internal.Optic

-- | Type synonym for a type-modifying lens.
type Lens s t a b = Optic A_Lens NoIx s t a b

-- | Type synonym for a type-preserving lens.
type Lens' s a = Optic' A_Lens NoIx s a

-- | Type synonym for a type-modifying van Laarhoven lens.
type LensVL s t a b = forall f. (Functor f, Coercible1 f) => (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven lens.
type LensVL' s a = LensVL s s a a

-- | Build a lens from a getter and a setter, which must respect the
-- well-formedness laws.
--
-- If you want to build a 'Lens' from the van Laarhoven representation, use
-- 'lensVL'.
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

-- | Strict version of 'Optics.Iso.equality'.
--
-- Useful for strictifying optics with lazy (irrefutable) pattern matching by
-- precomposition, e.g.
--
-- @
-- 'Data.Tuple.Optics._1'' = 'equality'' % 'Data.Tuple.Optics._1'
-- @
equality' :: Lens a b a b
equality' = lensVL ($!)
{-# INLINE equality' #-}

-- | Make a 'Lens' from two other lenses by executing them on their respective
-- halves of a product.
--
-- >>> (Left 'a', Right 'b') ^. alongside chosen chosen
-- ('a','b')
--
-- >>> (Left 'a', Right 'b') & alongside chosen chosen .~ ('c','d')
-- (Left 'c',Right 'd')
alongside
  :: (Is k A_Lens, Is l A_Lens)
  => Optic k is s  t  a  b
  -> Optic l js s' t' a' b'
  -> Lens (s, s') (t, t') (a, a') (b, b')
alongside l r = withLens l $ \getl setl ->
                withLens r $ \getr setr ->
  lens (\(s, s')         -> (getl s,   getr s'   ))
       (\(s, s') (b, b') -> (setl s b, setr s' b'))
{-# INLINE alongside #-}

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
