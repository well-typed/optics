# Migration guide to optics-0.4

## FunctorWithIndex instances

In `optics-0.4` the `FunctorWithIndex`, `FoldableWithIndex` and
`TraversableWithIndex` type classes have been migrated to a new package,

Beware: the `lens` package (versions `<5`) defines similar classes,
and will also migrate to use `indexed-traversable` classes. Therefore, you
might get duplicate instance errors if your package defines both.

If you define your own `FunctorWithIndex` etc. instances,
we recommend that you depend directly on the `indexed-traversable` package.
If you want to continue support `optics-0.3` users, you may write

```haskell
-- from indexed-traversable
import Data.Functor.WithIndex

-- from optics-core
import qualified Optics.Core as O

-- your (indexed) container
data MySeq a = ...

-- indexed-traversable instance
instance FunctorWithIndex     Int MySeq where imap = ...
instance FoldableWithIndex    Int MySeq where ifoldMap = ...
instance TraversableWithIndex Int MySeq where itraverse = ...

-- optics-core <0.4 instance, note the !
#if !MIN_VERSION_optics_core(0,4,0)
instance O.FunctorWithIndex     Int MySeq where imap = imap
instance O.FoldableWithIndex    Int MySeq where ifoldMap = ifoldMap
instance O.TraversableWithIndex Int MySeq where itraverse = itraverse
#endif
```

In other words, always provide `indexed-traversable` instances.
If your package depends on `optics(-core)` and allows `optics-0.3`,
you should additionally provide instances for `optics-0.3` type classes
that can reuse the `indexed-traversable` instances.
