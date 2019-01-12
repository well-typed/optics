module Optics.Operators.State (
    -- * State modifying optics
    (.=), (?=), (%=),
    -- * State modifying optics with passthrough
    (%%=),
    -- * Returning new value
    (<.=), (<?=), (<%=),
    -- * Returning old value
    (<<.=), (<<?=), (<<%=),
    -- * Passthrough
    PermeableOptic (..),
    ) where

import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State

import Optics
import Optics.Operators
import Optics.Internal.Passthrough

infix 4 .=, ?=, %=

(.=) :: (MonadState s m, Is k A_Setter) => Optic k i i  s s a b -> b -> m ()
o .= x = o %= const x

(?=) :: (MonadState s m, Is k A_Setter) => Optic k i i s s (Maybe a) (Maybe b) -> b -> m ()
o ?= x = o %= const (Just x)

(%=) :: (MonadState s m, Is k A_Setter) => Optic k i i s s a b -> (a -> b) -> m ()
o %= f = State.modify (o %~ f)

-------------------------------------------------------------------------------
-- Extra stuff
-------------------------------------------------------------------------------

infix 4 %%=
(%%=) :: (MonadState s m, PermeableOptic k r) => Optic k i i s s a b -> (a -> (r, b)) -> m (ViewResult k r)
o %%= f = State.state (passthrough o f)

-------------------------------------------------------------------------------
-- Returning new value
-------------------------------------------------------------------------------

infix 4 <.=, <?=, <%=

(<%=) :: (MonadState s m, PermeableOptic k b) => Optic k i i s s a b -> (a -> b) -> m (ViewResult k b)
o <%= f = o %%= \a -> let b = f a in (b, b)

(<?=) :: (MonadState s m, PermeableOptic k (Maybe b)) => Optic k i i s s (Maybe a) (Maybe b) -> b -> m (ViewResult k (Maybe b))
o <?= b = o <.= Just b

(<.=) :: (MonadState s m, PermeableOptic k b) => Optic k i i s s a b -> b -> m (ViewResult k b)
o <.= b = o <%= const b

-------------------------------------------------------------------------------
-- Returning old value
-------------------------------------------------------------------------------

infix 4 <<.=, <<?=, <<%=

(<<%=) :: (MonadState s m, PermeableOptic k a) => Optic k i i s s a b -> (a -> b) -> m (ViewResult k a)
o <<%= f = o %%= \a -> (a, f a)

(<<?=) :: (MonadState s m, PermeableOptic k (Maybe a)) => Optic k i i s s (Maybe a) (Maybe b) -> b -> m (ViewResult k (Maybe a))
o <<?= b = o <<.= Just b

(<<.=) :: (MonadState s m, PermeableOptic k a) => Optic k i i s s a b -> b -> m (ViewResult k a)
o <<.= b = o <<%= const b
