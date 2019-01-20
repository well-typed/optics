{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Test 'makeFields' on a field whose type has a data family. Unlike for type
-- families, for data families we do not generate type equality constraints, as
-- they are not needed to avoid the issue in #754.
--
-- This tests that the fix for #799 is valid by putting this in a module in
-- which UndecidableInstances is not enabled.
module Optics.TH.Tests.T799 where

import Optics.Lens
import Optics.TH

data family DF a
newtype instance DF Int = FooInt Int

data Bar = Bar { _barFoo :: DF Int }
makeFields ''Bar

checkBarFoo :: Lens' Bar (DF Int)
checkBarFoo = foo
