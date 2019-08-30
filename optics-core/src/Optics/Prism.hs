-- |
-- Module: Optics.Prism
-- Description: A generalised or first-class constructor.
--
-- A 'Prism' generalises the notion of a constructor (just as a
-- 'Optics.Lens.Lens' generalises the notion of a field).
--
module Optics.Prism
  (
  -- * Formation
    Prism
  , Prism'

  -- * Introduction
  , prism

  -- * Elimination
  -- | A 'Prism' is in particular an 'Optics.AffineFold.AffineFold', a
  -- 'Optics.Review.Review' and a 'Optics.Setter.Setter', therefore you can
  -- specialise types to obtain:
  --
  -- @
  -- 'Optics.AffineFold.preview' :: 'Prism' s t a b -> s -> Maybe a
  -- 'Optics.Review.review'  :: 'Prism' s t a b -> b -> t
  -- @
  --
  -- @
  -- 'Optics.Setter.over'    :: 'Prism' s t a b -> (a -> b) -> s -> t
  -- 'Optics.Setter.set'     :: 'Prism' s t a b ->       b  -> s -> t
  -- @

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Review.review'   ('prism' f g) ≡ f
  -- 'Optics.AffineTraversal.matching' ('prism' f g) ≡ g
  -- @

  -- * Well-formedness
  -- |
  --
  -- @
  -- 'Optics.AffineTraversal.matching' o ('Optics.Review.review' o b) ≡ 'Right' b
  -- 'Optics.AffineTraversal.matching' o s ≡ 'Right' a  =>  'Optics.Review.review' o a ≡ s
  -- @

  -- * Additional introduction forms
  -- | See "Data.Maybe.Optics" and "Data.Either.Optics" for 'Prism's for the
  -- corresponding types, and 'Optics.Cons.Core._Cons', 'Optics.Cons.Core._Snoc'
  -- and 'Optics.Empty.Core._Empty' for 'Prism's for container types.
  , prism'
  , only
  , nearly

  -- * Additional elimination forms
  , withPrism

  -- * Combinators
  , aside
  , without
  , below

  -- * Subtyping
  , A_Prism
  -- | <<diagrams/Prism.png Prism in the optics hierarchy>>
  )
  where

import Control.Monad
import Data.Bifunctor

import Optics.Internal.Concrete
import Optics.Internal.Optic
import Optics.Internal.Profunctor

-- | Type synonym for a type-modifying prism.
type Prism s t a b = Optic A_Prism NoIx s t a b

-- | Type synonym for a type-preserving prism.
type Prism' s a = Optic' A_Prism NoIx s a

-- | Build a prism from a constructor and a matcher, which must respect the
-- well-formedness laws.
--
-- If you want to build a 'Prism' from the van Laarhoven representation, use
-- @prismVL@ from the @optics-vl@ package.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism construct match = Optic $ dimap match (either id construct) . right'
{-# INLINE prism #-}

-- | This is usually used to build a 'Prism'', when you have to use an operation
-- like 'Data.Typeable.cast' which already returns a 'Maybe'.
prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

-- | Work with a 'Prism' as a constructor and a matcher.
withPrism
  :: Is k A_Prism
  => Optic k is s t a b
  -> ((b -> t) -> (s -> Either t a) -> r)
  -> r
withPrism o k = case getOptic (castOptic @A_Prism o) (Market id Right) of
  Market construct match -> k construct match
{-# INLINE withPrism #-}

----------------------------------------

-- | Use a 'Prism' to work over part of a structure.
aside :: Is k A_Prism => Optic k is s t a b -> Prism (e, s) (e, t) (e, a) (e, b)
aside k =
  withPrism k     $ \bt seta ->
  prism (fmap bt) $ \(e,s) ->
  case seta s of
    Left t  -> Left  (e,t)
    Right a -> Right (e,a)
{-# INLINE aside #-}

-- | Given a pair of prisms, project sums.
--
-- Viewing a 'Prism' as a co-'Optics.Lens.Lens', this combinator can be seen to
-- be dual to 'Optics.Lens.alongside'.
without
  :: (Is k A_Prism, Is l A_Prism)
  => Optic k is s t a b
  -> Optic l is u v c d
  -> Prism (Either s u) (Either t v) (Either a c) (Either b d)
without k =
  withPrism k         $ \bt seta k' ->
  withPrism k'        $ \dv uevc    ->
  prism (bimap bt dv) $ \su ->
  case su of
    Left s  -> bimap Left Left (seta s)
    Right u -> bimap Right Right (uevc u)
{-# INLINE without #-}

-- | Lift a 'Prism' through a 'Traversable' functor, giving a 'Prism' that
-- matches only if all the elements of the container match the 'Prism'.
below
  :: (Is k A_Prism, Traversable f)
  => Optic' k is s a
  -> Prism' (f s) (f a)
below k =
  withPrism k     $ \bt seta ->
  prism (fmap bt) $ \s ->
  case traverse seta s of
    Left _  -> Left s
    Right t -> Right t
{-# INLINE below #-}

-- | This 'Prism' compares for exact equality with a given value.
--
-- >>> only 4 # ()
-- 4
--
-- >>> 5 ^? only 4
-- Nothing
only :: Eq a => a -> Prism' a ()
only a = prism' (\() -> a) $ guard . (a ==)
{-# INLINE only #-}

-- | This 'Prism' compares for approximate equality with a given value and a
-- predicate for testing, an example where the value is the empty list and the
-- predicate checks that a list is empty (same as 'Optics.Empty._Empty' with the
-- 'Optics.Empty.AsEmpty' list instance):
--
-- >>> nearly [] null # ()
-- []
-- >>> [1,2,3,4] ^? nearly [] null
-- Nothing
--
-- @'nearly' [] 'Prelude.null' :: 'Prism'' [a] ()@
--
-- To comply with the 'Prism' laws the arguments you supply to @nearly a p@ are
-- somewhat constrained.
--
-- We assume @p x@ holds iff @x ≡ a@. Under that assumption then this is a valid
-- 'Prism'.
--
-- This is useful when working with a type where you can test equality for only
-- a subset of its values, and the prism selects such a value.
nearly :: a -> (a -> Bool) -> Prism' a ()
nearly a p = prism' (\() -> a) $ guard . p
{-# INLINE nearly #-}

-- $setup
-- >>> import Optics.Core
