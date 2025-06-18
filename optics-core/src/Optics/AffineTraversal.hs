-- |
-- Module: Optics.AffineTraversal
-- Description: A 'Optics.Traversal.Traversal' that applies to at most one element.
--
-- An 'AffineTraversal' is a 'Optics.Traversal.Traversal' that
-- applies to at most one element.
--
-- These arise most frequently as the composition of a
-- 'Optics.Lens.Lens' with a 'Optics.Prism.Prism'.
--
module Optics.AffineTraversal
  (
  -- * Formation
    AffineTraversal
  , AffineTraversal'

  -- * Introduction
  , atraversal

  -- * Elimination
  -- | An 'AffineTraversal' is in particular an 'Optics.AffineFold.AffineFold'
  -- and a 'Optics.Setter.Setter', therefore you can specialise types to obtain:
  --
  -- @
  -- 'Optics.AffineFold.preview' :: 'AffineTraversal' s t a b -> s -> Maybe a
  -- @
  --
  -- @
  -- 'Optics.Setter.over'    :: 'AffineTraversal' s t a b -> (a -> b) -> s -> t
  -- 'Optics.Setter.set'     :: 'AffineTraversal' s t a b ->       b  -> s -> t
  -- @
  , matching

  -- * Computation
  -- |
  --
  -- @
  -- 'matching' ('atraversal' f g) ≡ f
  -- 'Data.Either.isRight' (f s)  =>  'Optics.Setter.set' ('atraversal' f g) b s ≡ g s b
  -- @

  -- * Additional introduction forms
  -- | See 'Optics.Cons.Core._head', 'Optics.Cons.Core._tail',
  -- 'Optics.Cons.Core._init' and 'Optics.Cons.Core._last' for
  -- 'AffineTraversal's for container types.
  , unsafeFiltered

  -- * Additional elimination forms
  , withAffineTraversal

  , afailing

  -- * Subtyping
  , An_AffineTraversal
  -- | <<diagrams/AffineTraversal.png AffineTraversal in the optics hierarchy>>

  -- * van Laarhoven encoding
  , AffineTraversalVL
  , AffineTraversalVL'
  , atraversalVL
  , atraverseOf
  )
  where

import Data.Profunctor.Indexed

import Optics.Internal.Optic
import Optics.Internal.Utils

-- | Type synonym for a type-modifying affine traversal.
type AffineTraversal s t a b = Optic An_AffineTraversal NoIx s t a b

-- | Type synonym for a type-preserving affine traversal.
type AffineTraversal' s a = Optic' An_AffineTraversal NoIx s a

-- | Type synonym for a type-modifying van Laarhoven affine traversal.
--
-- Note: this isn't exactly van Laarhoven representation as there is
-- no @Pointed@ class (which would be a superclass of 'Applicative'
-- that contains 'pure' but not '<*>'). You can interpret the first
-- argument as a dictionary of @Pointed@ that supplies the @point@
-- function (i.e. the implementation of 'pure').
--
-- A 'Optics.Traversal.TraversalVL' has 'Applicative' available and
-- hence can combine the effects arising from multiple elements using
-- '<*>'. In contrast, an 'AffineTraversalVL' has no way to combine
-- effects from multiple elements, so it must act on at most one
-- element.  (It can act on none at all thanks to the availability of
-- @point@.)
--
type AffineTraversalVL s t a b =
  forall f. Functor f => (forall r. r -> f r) -> (a -> f b) -> s -> f t

-- | Type synonym for a type-preserving van Laarhoven affine traversal.
type AffineTraversalVL' s a = AffineTraversalVL s s a a

-- | Build an affine traversal from a matcher and an updater.
--
-- If you want to build an 'AffineTraversal' from the van Laarhoven
-- representation, use 'atraversalVL'.
atraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
atraversal match update = Optic $
  -- Do not define atraversal in terms of atraversalVL, mixing profunctor-style
  -- definitions with VL style implementation can lead to subpar generated code.
  dimap (\s -> (match s, update s))
        (\(etb, f) -> either id f etb)
  . first'
  . right'
{-# INLINE atraversal #-}

-- | Work with an affine traversal as a matcher and an updater.
withAffineTraversal
  :: Is k An_AffineTraversal
  => Optic k is s t a b
  -> ((s -> Either t a) -> (s -> b -> t) -> r)
  -> r
withAffineTraversal o = \k ->
  case getOptic (castOptic @An_AffineTraversal o) (AffineMarket (\_ b -> b) Right) of
    AffineMarket update match -> k match update
{-# INLINE withAffineTraversal #-}

-- | Build an affine traversal from the van Laarhoven representation.
--
-- Example:
--
-- >>> :{
-- azSnd = atraversalVL $ \point f ab@(a, b) ->
--   if a >= 'a' && a <= 'z'
--   then (a, ) <$> f b
--   else point ab
-- :}
--
-- >>> preview azSnd ('a', "Hi")
-- Just "Hi"
--
-- >>> preview azSnd ('@', "Hi")
-- Nothing
--
-- >>> over azSnd (++ "!!!") ('f', "Hi")
-- ('f',"Hi!!!")
--
-- >>> set azSnd "Bye" ('Y', "Hi")
-- ('Y',"Hi")
--
atraversalVL :: AffineTraversalVL s t a b -> AffineTraversal s t a b
atraversalVL f = Optic (visit f)
{-# INLINE atraversalVL #-}

-- | Traverse over the target of an 'AffineTraversal' and compute a
-- 'Functor'-based answer.
--
-- @since 0.3
atraverseOf
  :: (Is k An_AffineTraversal, Functor f)
  => Optic k is s t a b
  -> (forall r. r -> f r) -> (a -> f b) -> s -> f t
atraverseOf o point =
  runStarA . getOptic (castOptic @An_AffineTraversal o) . StarA point
{-# INLINE atraverseOf #-}

-- | Retrieve the value targeted by an 'AffineTraversal' or return the original
-- value while allowing the type to change if it does not match.
--
-- @
-- 'Optics.AffineFold.preview' o ≡ 'either' ('const' 'Nothing') 'id' . 'matching' o
-- @
matching :: Is k An_AffineTraversal => Optic k is s t a b -> s -> Either t a
matching o = withAffineTraversal o $ \match _ -> match
{-# INLINE matching #-}

afailing
  :: (Is k An_AffineTraversal, Is l An_AffineTraversal)
  => Optic k is s t a b
  -> Optic l js s t a b
  -> AffineTraversal s t a b
afailing a b = atraversalVL $ \point f s ->
  let OrT visited fu = atraverseOf a (OrT False . point) (wrapOrT . f) s
  in if visited
     then fu
     else atraverseOf b point f s
infixl 3 `afailing` -- Same as (<|>)
{-# INLINE afailing #-}

-- | Filter result(s) of a traversal that don't satisfy a predicate.
--
-- /Note:/ This is /not/ a legal 'Optics.Traversal.Traversal', unless you are
-- very careful not to invalidate the predicate on the target.
--
-- As a counter example, consider that given @evens = 'unsafeFiltered' 'even'@
-- the second 'Optics.Traversal.Traversal' law is violated:
--
-- @
-- 'Optics.Setter.over' evens 'succ' '.' 'Optics.over' evens 'succ' '/=' 'Optics.Setter.over' evens ('succ' '.' 'succ')
-- @
--
-- So, in order for this to qualify as a legal 'Optics.Traversal.Traversal' you
-- can only use it for actions that preserve the result of the predicate!
--
-- For a safe variant see 'Optics.IxTraversal.indices' (or
-- 'Optics.AffineFold.filtered' for read-only optics).
--
unsafeFiltered :: (a -> Bool) -> AffineTraversal' a a
unsafeFiltered p = atraversalVL (\point f a -> if p a then f a else point a)
{-# INLINE unsafeFiltered #-}

-- $setup
-- >>> import Optics.Core
