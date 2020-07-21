{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Optics.Monoidal (
  After(..)
, AfterJoin
) where

import Optics.Each.Core
import Optics.Fold
import Optics.Indexed.Core
import Optics.Internal.Optic.Types
import Optics.Lens
import Optics.Optic
import Optics.Setter
import Optics.Traversal

-- Essentially this collapses the hierarchy into
-- Traversal, Fold, and Setter,
-- and then takes the usual Join
type family AfterJoin (k :: OpticKind) (l :: OpticKind) where
  AfterJoin k l = Join A_Traversal (Join k l)

infixr 6 <++>
class After is m where
  (<++>)  :: (Is k m, Is l m, m ~ AfterJoin k l)
          => Optic' k is s a -> Optic' l is s a
          -> Optic' m is s a

instance After NoIx A_Traversal where
  t1 <++> t2 = split (partsOf t1) (partsOf t2) % noIx each % traversed
-- instance After (WithIx ix) A_Traversal where  -- TO BE WRITTEN
--  t1 <++> t2 = split (partsOf t1) (partsOf t2) % noIx each % traversed

instance After NoIx A_Fold where
  (<++>) = summing
instance After (WithIx ix) A_Fold where
  (<++>) = isumming

instance After NoIx A_Setter where
  s1 <++> s2 = sets (\f s -> over s2 f (over s1 f s))
instance After (WithIx ix) A_Setter where
  s1 <++> s2 = isets (\f s -> iover s2 f (iover s1 f s))