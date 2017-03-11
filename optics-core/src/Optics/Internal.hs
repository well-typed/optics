{-# LANGUAGE DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           #-}

module Optics.Internal where

import Control.Applicative (Const(..))
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity(..))
import GHC.Exts (Constraint)


-- | Wrapper newtype for the whole family of vaguely lens-like things.
-- The first type parameter @k@ identifies the particular flavour
-- (e.g. 'A_Lens' or 'A_Traversal').
newtype Optic k s t a b = Optic { getOptic :: Optic_ k s t a b }

-- | By choosing different instantiations for @p@ and @f@, along with
-- different constraints, we can make this represent any flavour of
-- optic.  I wish I understood this type better.
type Optic_ k s t a b = forall p f . Constraints k p f => p a (f b) -> p s (f t)

-- | Specific constraints on @p@ and @f@ that are needed for lens-like
-- things of flavour @k@.
type family Constraints (k :: *) (p :: * -> * -> *) (f :: * -> *) :: Constraint

-- | A simple 'Optic' without the ability to do type-changing update
-- (or whatever the equivalent is).
type Optic' k s a = Optic k s s a a


-- | Hooray, subtyping!  This gives the relationship between flavours,
-- e.g. we have @Is A_Lens A_Traversal@ but not @Is A_Traversal A_Lens@.
-- Unfortunately we have to give O(n^2) instances of this class.
class Is k l where
  implies :: proxy k l p f -> (Constraints k p f => r) -> (Constraints l p f => r)

-- | Of course, every flavour can be used as itself.
instance Is k k where
  implies _ = id

-- | Explicit cast from one optic flavour to another.  This is the
-- identity function, modulo some constraint jiggery-pokery.
sub :: forall k l s t a b . Is k l => Optic k s t a b -> Optic l s t a b
sub (Optic o) = Optic (implies' o)
  where
    implies' :: forall p f . (Constraints k p f => p a (f b) -> p s (f t))
                          -> (Constraints l p f => p a (f b) -> p s (f t))
    implies' x = implies (undefined :: proxy k l p f) x


-- | In order to define composition of optics, we need a way to talk
-- about the join (or should it be meet?) of two flavours in the
-- subtyping lattice, i.e. their least upper bound.
class (Is k m, Is l m) => Join k l m | k l -> m

instance Join k k k

-- | Compose two optics, automatically casting them to the appropriate
-- supertype.
(%) :: Join k l m => Optic k s t u v -> Optic l u v a b -> Optic m s t a b
o % o' = sub o %% sub o'

-- | Compose two optics that already have the same flavour.  It's not
-- obvious whether this is needed once one has defined '(%)'.
(%%) :: Optic k s t u v -> Optic k u v a b -> Optic k s t a b
Optic o %% Optic o' = Optic (o . o')



-- This lot is all copied from elsewhere, to keep this module
-- free-standing.

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
instance Contravariant (Const r) where
  contramap _ (Const x) = Const x

class Profunctor (p :: * -> * -> *) where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
instance Profunctor (->) where
  dimap f g k = g . k . f

class Profunctor p => Choice (p :: * -> * -> *) where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
instance Choice (->) where
  left'  f = either (Left . f) Right
  right' f = either Left (Right . f)



data A_Fold
type instance Constraints A_Fold p f = (p ~ (->), Contravariant f, Applicative f)
type Fold s a = Optic' A_Fold s a

toFold :: Is k A_Fold => Optic' k s a -> Fold s a
toFold = sub


data A_Setter
type instance Constraints A_Setter p f = (p ~ (->), f ~ Identity)
type Setter s t a b = Optic A_Setter s t a b
type Setter' s a = Optic' A_Setter s a

toSetter :: Is k A_Setter => Optic k s t a b -> Setter s t a b
toSetter = sub

sets :: ((a -> b) -> (s -> t)) -> Setter s t a b
sets f = Optic (coerce f)

mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

over :: forall k s t a b . Is k A_Setter => Optic k s t a b -> (a -> b) -> s -> t
over o = coerce (getOptic (sub o :: Setter s t a b))


data A_Getter
type instance Constraints A_Getter p f = (p ~ (->), Contravariant f, Functor f)
type Getter s a = Optic' A_Getter s a

view :: Is k A_Getter => Optic' k s a -> s -> a
view o s = getConst (getOptic (toGetter o) Const s)

to :: (s -> a) -> Getter s a
to f = Optic (\ q s -> contramap f (q (f s)))

toGetter :: Is k A_Getter => Optic' k s a -> Getter s a
toGetter = sub


data A_Traversal
type instance Constraints A_Traversal p f = (p ~ (->), Applicative f)
type Traversal s t a b = Optic A_Traversal s t a b
type Traversal' s a = Optic' A_Traversal s a

toTraversal :: Is k A_Traversal => Optic k s t a b -> Traversal s t a b
toTraversal = sub

traversalOf :: forall k s t a b . Is k A_Traversal => Optic k s t a b -> Optic_ A_Traversal s t a b
traversalOf = getOptic . toTraversal

mkTraversal :: Optic_ A_Traversal s t a b -> Traversal s t a b
mkTraversal = Optic


data A_Lens
type instance Constraints A_Lens p f = (p ~ (->), Functor f)
type Lens s t a b = Optic A_Lens s t a b
type Lens' s a = Optic' A_Lens s a

toLens :: Is k A_Lens => Optic k s t a b -> Lens s t a b
toLens = sub

mkLens :: Optic_ A_Lens s t a b -> Lens s t a b
mkLens = Optic

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = mkLens (\ f s -> set s <$> f (get s))

_1 :: Lens (a,y) (b,y) a b
_1 = lens fst (\ (_,y) x -> (x,y))


data A_Prism
type instance Constraints A_Prism p f = (Choice p, Applicative f)
type Prism s t a b = Optic A_Prism s t a b
type Prism' s a = Optic A_Prism s a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism f g = Optic $ \ p -> dimap g (either pure (fmap f)) (right' p)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right (either (Left . Left) Right)


data An_Iso
type instance Constraints An_Iso p f = (Profunctor p, Functor f)
type Iso s t a b = Optic An_Iso s t a b
type Iso' s a = Optic' An_Iso s a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = Optic (\ x -> dimap f (fmap g) x)



instance Is A_Getter A_Fold where
  implies _ = id

instance Is A_Traversal A_Fold where
  implies _ = id
instance Is A_Traversal A_Setter where
  implies _ = id

instance Is A_Lens A_Fold where
  implies _ = id
instance Is A_Lens A_Setter where
  implies _ = id
instance Is A_Lens A_Getter where
  implies _ = id
instance Is A_Lens A_Traversal where
  implies _ = id

instance Is A_Prism A_Fold where
  implies _ = id
instance Is A_Prism A_Setter where
  implies _ = id
instance Is A_Prism A_Traversal where
  implies _ = id

instance Is An_Iso A_Fold where
  implies _ = id
instance Is An_Iso A_Setter where
  implies _ = id
instance Is An_Iso A_Getter where
  implies _ = id
instance Is An_Iso A_Traversal where
  implies _ = id
instance Is An_Iso A_Lens where
  implies _ = id
instance Is An_Iso A_Prism where
  implies _ = id



instance Join A_Fold      A_Getter     A_Fold
instance Join A_Fold      A_Traversal  A_Fold
instance Join A_Fold      A_Lens       A_Fold
instance Join A_Fold      A_Prism      A_Fold
instance Join A_Fold      An_Iso       A_Fold

instance Join A_Setter    A_Traversal  A_Setter
instance Join A_Setter    A_Lens       A_Setter
instance Join A_Setter    A_Prism      A_Setter
instance Join A_Setter    An_Iso       A_Setter

instance Join A_Getter    A_Fold       A_Fold
instance Join A_Getter    A_Traversal  A_Fold
instance Join A_Getter    A_Lens       A_Getter
instance Join A_Getter    A_Prism      A_Fold
instance Join A_Getter    An_Iso       A_Getter

instance Join A_Traversal A_Fold       A_Fold
instance Join A_Traversal A_Setter     A_Setter
instance Join A_Traversal A_Getter     A_Fold
instance Join A_Traversal A_Lens       A_Traversal
instance Join A_Traversal A_Prism      A_Traversal
instance Join A_Traversal An_Iso       A_Traversal

instance Join A_Lens      A_Fold       A_Fold
instance Join A_Lens      A_Setter     A_Setter
instance Join A_Lens      A_Getter     A_Getter
instance Join A_Lens      A_Traversal  A_Traversal
instance Join A_Lens      A_Prism      A_Traversal
instance Join A_Lens      An_Iso       A_Lens


instance Join A_Prism     A_Fold       A_Fold
instance Join A_Prism     A_Setter     A_Setter
instance Join A_Prism     A_Getter     A_Fold
instance Join A_Prism     A_Traversal  A_Traversal
instance Join A_Prism     A_Lens       A_Traversal
instance Join A_Prism     An_Iso       A_Prism

instance Join An_Iso      A_Fold       A_Fold
instance Join An_Iso      A_Setter     A_Setter
instance Join An_Iso      A_Getter     A_Getter
instance Join An_Iso      A_Traversal  A_Traversal
instance Join An_Iso      A_Lens       A_Lens
instance Join An_Iso      A_Prism      A_Prism


-- | A constraint that can never be satisfied (accompanied by a
-- helpful witness to anything you like).
class Absurd where
  absurd :: a

-- | In order to get nice error messages, we complete the type lattice
-- with a universal supertype, whose constraints can never be
-- satisfied.
data Bogus
type instance Constraints Bogus p f = Absurd
instance Is k Bogus where
  implies = absurd

-- | This typeclass has no instances, and is used so that we get
-- suitable unsolved constraint errors when attempting to compose
-- flavours that do not have a (non-'Bogus') common supertype.
--
-- For example, if you try to compose a fold with a setter then you
-- will get an error
--
-- > Could not deduce (CanCompose A_Fold A_Setter)
class Absurd => CanCompose k l

instance CanCompose A_Fold   A_Setter => Join A_Fold   A_Setter Bogus
instance CanCompose A_Setter A_Fold   => Join A_Setter A_Fold   Bogus
instance CanCompose A_Setter A_Getter => Join A_Setter A_Getter Bogus
instance CanCompose A_Getter A_Setter => Join A_Getter A_Setter Bogus


-- | Composing a lens and a traversal yields a traversal
comp1 :: Traversable t => Optic A_Traversal (t a, y) (t b, y) a b
comp1 = _1 % mkTraversal traverse

-- | Composing two lenses yields a lens
comp2 :: Optic A_Lens ((a, y), y1) ((b, y), y1) a b
comp2 = _1 % _1

-- | Composing a getter and a lens yields a getter
comp3 :: Optic A_Getter ((b, y), b1) ((b, y), b1) b b
comp3 = to fst % _1

-- | Composing a prism and a lens yields a traversal
comp4 :: Optic A_Traversal (Either c (a, y)) (Either c (b, y)) a b
comp4 = _Right % _1

-- | An iso can be used as a getter
eg1 :: Int
eg1 = view (iso (+ 1) (\ x -> x - 1)) 5

-- | A lens can be used as a getter
eg2 :: (a, b) -> a
eg2 = view _1


-- These don't typecheck, as one would expect:
--   to fst % mapped  -- Cannot compose a getter with a setter
--   toLens (to fst)  -- Cannot use a getter as a lens
