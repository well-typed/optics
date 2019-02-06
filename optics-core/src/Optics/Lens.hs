-- | 'Lens' is a generalised or first-class *field*.
--
-- If we have a value @s :: S@, and a @l :: 'Lens'' S A@,
-- we can /get/ the "field value" of type @A@ using 'view'.
-- We can also /update/ (or /put/ or /set/) the value using 'over' (or 'set').
--
-- >>> data Human = Human { _name :: String, _location :: String } deriving Show
-- >>> let human = Human "Bob" "London"
--
-- We can make a 'Lens' for @_name@ field:
--
-- >>> let name = lens _name $ \s x -> s { _name = x }
--
-- Which we can use as a 'Getter'
--
-- >>> view name human
-- "Bob"
--
-- or a 'Setter'
--
-- >>> set name "Robert" human
-- Human {_name = "Robert", _location = "London"}
--
module Optics.Lens
  (
  -- * Formation
    A_Lens
  , Lens
  , Lens'

  -- * Introduction
  , lens
  , withLens
  , toLens

  -- * Elimination
  -- | 'Lens' is a 'Setter' and a 'Getter', therefore you can
  --
  -- @
  -- 'view' :: 'Lens' i s t a b -> s -> a
  -- 'set'  :: 'Lens' i s t a b -> b -> s -> t
  -- 'over' :: 'Lens' i s t a b -> (a -> b) -> s -> t
  -- @
  --

  -- * Computation
  -- |
  --
  -- @
  -- 'view' ('lens' f g)   s = f s
  -- 'set'  ('lens' f g) a s = g s a
  -- @

  -- * Well-formedness
  -- |
  --
  -- * __GetPut__: You get back what you put in:
  --
  --     @
  --     view l (set l v s) = v
  --     @
  --
  -- * __PutGet__: Putting back what you got doesn’t change anything:
  --
  --     @
  --     set l (view l s) s = s
  --     @
  --
  -- * __PutPut__: Setting twice is the same as setting once:
  --
  --     @
  --     set l v' (set l v s) = set l v' s
  --     @
  --

  -- * van Laarhoven encoding
  , LensVL
  , LensVL'
  , lensVL
  , toLensVL
  , withLensVL

  -- * Re-exports
  , module Optics.Optic
  )
  where

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

-- | Explicitly cast an optic to a lens.
toLens :: Is k A_Lens => Optic k is s t a b -> Optic A_Lens is s t a b
toLens = castOptic
{-# INLINE toLens #-}

-- | Build a lens from a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = Optic $
  -- Do not define lens in terms of lensVL, mixing profunctor-style definitions
  -- with VL style implementation leads to subpar generated code, i.e. updating
  -- often gets and then sets as opposed to updating in place.
  dimap (\s -> (get s, s))
        (\(b, s) -> set s b)
  . first'
{-# INLINE lens #-}

-- | Work with a lens as a getter and a setter.
withLens
  :: Is k A_Lens
  => Optic k is s t a b
  -> ((s -> a) -> (s -> b -> t) -> r)
  -> r
withLens o k = case getOptic (toLens o) $ Store id (\_ -> id) of
  Store get set -> k get set
{-# INLINE withLens #-}

-- | Build a lens from the van Laarhoven representation.
lensVL :: LensVL s t a b -> Lens s t a b
lensVL l = Optic (linear l)
{-# INLINE lensVL #-}

-- | Convert a lens to the van Laarhoven representation.
toLensVL :: Is k A_Lens => Optic k is s t a b -> LensVL s t a b
toLensVL o = runStar #. getOptic (toLens o) .# Star
{-# INLINE toLensVL #-}

-- | Work with a lens in the van Laarhoven representation.
withLensVL
  :: Is k A_Lens
  => Optic k is s t a b
  -> (LensVL s t a b -> r)
  -> r
withLensVL o k = k (toLensVL o)
{-# INLINE withLensVL #-}

-- $setup
-- >>> import Optics.Core
