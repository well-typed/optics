-- |
-- Module: Optics.Iso
-- Description: Translates between types with the same structure.
--
-- An 'Iso'morphism expresses the fact that two types have the
-- same structure, and hence can be converted from one to the other in
-- either direction.
--
module Optics.Iso
  (
  -- * Formation
    Iso
  , Iso'

  -- * Introduction
  , iso

  -- * Elimination
  -- | An 'Iso' is in particular a 'Optics.Getter.Getter', a
  -- 'Optics.Review.Review' and a 'Optics.Setter.Setter', therefore you can
  -- specialise types to obtain:
  --
  -- @
  -- 'Optics.Getter.view'   :: 'Iso'' s a -> s -> a
  -- 'Optics.Review.review' :: 'Iso'' s a -> a -> s
  -- @
  --
  -- @
  -- 'Optics.Setter.over'   :: 'Iso' s t a b -> (a -> b) -> s -> t
  -- 'Optics.Setter.set'    :: 'Iso' s t a b ->       b  -> s -> t
  -- @
  --
  -- If you want to 'Optics.Getter.view' a type-modifying 'Iso' that is
  -- insufficiently polymorphic to be used as a type-preserving 'Iso'', use
  -- 'Optics.ReadOnly.getting':
  --
  -- @
  -- 'Optics.Getter.view' . 'Optics.ReadOnly.getting' :: 'Iso' s t a b -> s -> a
  -- @

  -- * Computation
  -- |
  --
  -- @
  -- 'Optics.Getter.view'   ('iso' f g) ≡ f
  -- 'Optics.Review.review' ('iso' f g) ≡ g
  -- @

  -- * Well-formedness
  -- | The functions translating back and forth must be mutually inverse:
  --
  -- @
  -- 'Optics.Getter.view' i . 'Optics.Review.review' i ≡ 'id'
  -- 'Optics.Review.review' i . 'Optics.Getter.view' i ≡ 'id'
  -- @

  -- * Additional introduction forms
  , equality
  , simple
  , coerced
  , coercedTo
  , coerced1
  , non
  , non'
  , anon
  , curried
  , uncurried
  , flipped
  , involuted
  , Swapped(..)

  -- * Additional elimination forms
  , withIso
  , au
  , under

  -- * Combinators
  -- | The 'Optics.Re.re' combinator can be used to reverse an 'Iso', and the
  -- 'Optics.Mapping.mapping' combinator to lift an 'Iso' to an 'Iso' on
  -- functorial values.
  --
  -- @
  -- 'Optics.Re.re'      ::                           'Iso' s t a b -> 'Iso' b a t s
  -- 'Optics.Mapping.mapping' :: (Functor f, Functor g) => 'Iso' s t a b -> 'Iso' (f s) (g t) (f a) (g b)
  -- @

  -- * Subtyping
  , An_Iso
  -- | <<diagrams/Iso.png Iso in the optics hierarchy>>
  )
  where

import Data.Tuple
import Data.Bifunctor
import Data.Coerce
import Data.Maybe

import Data.Profunctor.Indexed

import Optics.AffineFold
import Optics.Prism
import Optics.Review
import Optics.Internal.Optic

-- | Type synonym for a type-modifying iso.
type Iso s t a b = Optic An_Iso NoIx s t a b

-- | Type synonym for a type-preserving iso.
type Iso' s a = Optic' An_Iso NoIx s a

-- | Build an iso from a pair of inverse functions.
--
-- If you want to build an 'Iso' from the van Laarhoven representation, use
-- @isoVL@ from the @optics-vl@ package.
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (dimap f g)
{-# INLINE iso #-}

-- | Extract the two components of an isomorphism.
withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o k = case getOptic o (Exchange id id) of
  Exchange sa bt -> k sa bt
{-# INLINE withIso #-}

-- | Based on @ala@ from Conor McBride's work on Epigram.
--
-- This version is generalized to accept any 'Iso', not just a @newtype@.
--
-- >>> au (coerced1 @Sum) foldMap [1,2,3,4]
-- 10
--
-- You may want to think of this combinator as having the following, simpler
-- type:
--
-- @
-- au :: 'Iso' s t a b -> ((b -> t) -> e -> s) -> e -> a
-- @
au :: Functor f => Iso s t a b -> ((b -> t) -> f s) -> f a
au k = withIso k $ \sa bt f -> sa <$> f bt
{-# INLINE au #-}

-- | The opposite of working 'Optics.Setter.over' a 'Optics.Setter.Setter' is
-- working 'under' an isomorphism.
--
-- @
-- 'under' ≡ 'Optics.Setter.over' '.' 'Optics.Re.re'
-- @
under :: Iso s t a b -> (t -> s) -> b -> a
under k = withIso k $ \sa bt ts -> sa . ts . bt
{-# INLINE under #-}

----------------------------------------
-- Isomorphisms

-- | Capture type constraints as an isomorphism.
--
-- /Note:/ This is the identity optic:
--
-- >>> :t view equality
-- view equality :: a -> a
equality :: (s ~ a, t ~ b) => Iso s t a b
equality = Optic id
{-# INLINE equality #-}

-- | Proof of reflexivity.
simple :: Iso' a a
simple = Optic id
{-# INLINE simple #-}

-- | Data types that are representationally equal are isomorphic.
--
-- >>> view coerced 'x' :: Identity Char
-- Identity 'x'
--
coerced :: (Coercible s a, Coercible t b) => Iso s t a b
coerced = Optic (lcoerce' . rcoerce')
{-# INLINE coerced #-}

-- | Type-preserving version of 'coerced' with type parameters rearranged for
-- TypeApplications.
--
-- >>> newtype MkInt = MkInt Int deriving Show
--
-- >>> over (coercedTo @Int) (*3) (MkInt 2)
-- MkInt 6
--
coercedTo :: forall a s. Coercible s a => Iso' s a
coercedTo = Optic (lcoerce' . rcoerce')
{-# INLINE coercedTo #-}

-- | Special case of 'coerced' for trivial newtype wrappers.
--
-- >>> over (coerced1 @Identity) (++ "bar") (Identity "foo")
-- Identity "foobar"
--
coerced1
  :: forall f s a. (Coercible s (f s), Coercible a (f a))
  => Iso (f s) (f a) s a
coerced1 = Optic (lcoerce' . rcoerce')
{-# INLINE coerced1 #-}

-- | If @v@ is an element of a type @a@, and @a'@ is @a@ sans the element @v@,
-- then @'non' v@ is an isomorphism from @'Maybe' a'@ to @a@.
--
-- @
-- 'non' ≡ 'non'' '.' 'only'
-- @
--
-- Keep in mind this is only a real isomorphism if you treat the domain as being
-- @'Maybe' (a sans v)@.
--
-- This is practically quite useful when you want to have a 'Data.Map.Map' where
-- all the entries should have non-zero values.
--
-- >>> Map.fromList [("hello",1)] & at "hello" % non 0 %~ (+2)
-- fromList [("hello",3)]
--
-- >>> Map.fromList [("hello",1)] & at "hello" % non 0 %~ (subtract 1)
-- fromList []
--
-- >>> Map.fromList [("hello",1)] ^. at "hello" % non 0
-- 1
--
-- >>> Map.fromList [] ^. at "hello" % non 0
-- 0
--
-- This combinator is also particularly useful when working with nested maps.
--
-- /e.g./ When you want to create the nested 'Data.Map.Map' when it is missing:
--
-- >>> Map.empty & at "hello" % non Map.empty % at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- and when have deleting the last entry from the nested 'Data.Map.Map' mean
-- that we should delete its entry from the surrounding one:
--
-- >>> Map.fromList [("hello", Map.fromList [("world","!!!")])] & at "hello" % non Map.empty % at "world" .~ Nothing
-- fromList []
--
-- It can also be used in reverse to exclude a given value:
--
-- >>> non 0 # rem 10 4
-- Just 2
--
-- >>> non 0 # rem 10 5
-- Nothing
--
-- @since 0.2
non :: Eq a => a -> Iso' (Maybe a) a
non = non' . only
{-# INLINE non #-}

-- | @'non'' p@ generalizes @'non' (p # ())@ to take any unit 'Prism'
--
-- This function generates an isomorphism between @'Maybe' (a | 'isn't' p a)@
-- and @a@.
--
-- >>> Map.singleton "hello" Map.empty & at "hello" % non' _Empty % at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- >>> Map.fromList [("hello", Map.fromList [("world","!!!")])] & at "hello" % non' _Empty % at "world" .~ Nothing
-- fromList []
--
-- @since 0.2
non' :: Prism' a () -> Iso' (Maybe a) a
non' p = iso (fromMaybe def) go where
  def                = review p ()
  go b | p `isn't` b = Just b
       | otherwise   = Nothing
{-# INLINE non' #-}

-- | @'anon' a p@ generalizes @'non' a@ to take any value and a predicate.
--
-- @
-- 'anon' a ≡ 'non'' '.' 'nearly' a
-- @
--
-- This function assumes that @p a@ holds @'True'@ and generates an isomorphism
-- between @'Maybe' (a | 'not' (p a))@ and @a@.
--
-- >>> Map.empty & at "hello" % anon Map.empty Map.null % at "world" ?~ "!!!"
-- fromList [("hello",fromList [("world","!!!")])]
--
-- >>> Map.fromList [("hello", Map.fromList [("world","!!!")])] & at "hello" % anon Map.empty Map.null % at "world" .~ Nothing
-- fromList []
--
-- @since 0.2
anon :: a -> (a -> Bool) -> Iso' (Maybe a) a
anon a = non' . nearly a
{-# INLINE anon #-}

-- | The canonical isomorphism for currying and uncurrying a function.
--
-- @
-- 'curried' = 'iso' 'curry' 'uncurry'
-- @
--
-- >>> view curried fst 3 4
-- 3
--
curried :: Iso ((a, b) -> c) ((d, e) -> f) (a -> b -> c) (d -> e -> f)
curried = iso curry uncurry
{-# INLINE curried #-}

-- | The canonical isomorphism for uncurrying and currying a function.
--
-- @
-- 'uncurried' = 'iso' 'uncurry' 'curry'
-- @
--
-- @
-- 'uncurried' = 'Optics.Re.re' 'curried'
-- @
--
-- >>> (view uncurried (+)) (1,2)
-- 3
--
uncurried :: Iso (a -> b -> c) (d -> e -> f) ((a, b) -> c) ((d, e) -> f)
uncurried = iso uncurry curry
{-# INLINE uncurried #-}

-- | The isomorphism for flipping a function.
--
-- >>> (view flipped (,)) 1 2
-- (2,1)
--
flipped :: Iso (a -> b -> c) (a' -> b' -> c') (b -> a -> c) (b' -> a' -> c')
flipped = iso flip flip
{-# INLINE flipped #-}

-- | Given a function that is its own inverse, this gives you an 'Iso' using it
-- in both directions.
--
-- @
-- 'involuted' ≡ 'Control.Monad.join' 'iso'
-- @
--
-- >>> "live" ^. involuted reverse
-- "evil"
--
-- >>> "live" & involuted reverse %~ ('d':)
-- "lived"
involuted :: (a -> a) -> Iso' a a
involuted a = iso a a
{-# INLINE involuted #-}

-- | This class provides for symmetric bifunctors.
class Data.Bifunctor.Bifunctor p => Swapped p where
  -- |
  -- @
  -- 'swapped' '.' 'swapped' ≡ 'id'
  -- 'first' f '.' 'swapped' = 'swapped' '.' 'second' f
  -- 'second' g '.' 'swapped' = 'swapped' '.' 'first' g
  -- 'bimap' f g '.' 'swapped' = 'swapped' '.' 'bimap' g f
  -- @
  --
  -- >>> view swapped (1,2)
  -- (2,1)
  --
  swapped :: Iso (p a b) (p c d) (p b a) (p d c)

instance Swapped (,) where
  swapped = iso swap swap
  {-# INLINE swapped #-}

instance Swapped Either where
  swapped = iso (either Right Left) (either Right Left)
  {-# INLINE swapped #-}

-- $setup
-- >>> import qualified Data.Map as Map
-- >>> import Data.Functor.Identity
-- >>> import Data.Monoid
-- >>> import Optics.Core
