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
  , toLens

  -- * Elimination
  -- | 'Lens' is a 'Setter' and a 'Getter', therefore you can
  --
  -- @
  -- 'view1' :: 'Lens' i s t a b -> s -> a
  -- 'set'   :: 'Lens' i s t a b -> b -> s -> t
  -- 'over'  :: 'Lens' i s t a b -> (a -> b) -> s -> t
  -- @
  --

  -- * Computation
  -- |
  --
  -- @
  -- 'view1' ('lens' f g)   s = f s
  -- 'set'   ('lens' f g) a s = g s a
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
  , module Optics.Optic
  )
  where

import Optics.Internal.Lens
import Optics.Internal.Optic
import Optics.Optic

-- $setup
-- >>> import Optics
