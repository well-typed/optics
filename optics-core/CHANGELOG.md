# optics-core-0.4 (2021-mm-dd)
* GHC-9.0 support
* The `FunctorWithIndex`, `FoldableWithIndex` and `TraversableWithIndex` type classes
  have been migrated to a new package,
  [`indexed-traversable`](https://hackage.haskell.org/package/indexed-traversable).

  Beware: the `lens` package (versions <5) defines similar classes,
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

* Add `adjoin` and `iadjoin`
* Add `both`
* Generalize types of `generic` and `generic1`
* Make `chosen` an indexed lens to see which value is traversed

# optics-core-0.3.0.1 (2020-08-05)
* Add INLINE pragmas to `atraverseOf_`, `iaTraverseOf_` and `ignored`
* Improve error message in catch-all `GeneralLabelOptic` instance
* Make GHC optimize away profunctor type classes when profiling is enabled
* Improve documentation of `Optics.Label`:
  - Add guide on how to effectively use labels as optics
  - Restructure existing sections

# optics-core-0.3 (2020-04-15)
* GHC-8.10 support
* Add `filteredBy` and `unsafeFilteredBy`
* Add `FunctorWithIndex`, `FoldableWithIndex` and `TraversableWithIndex`
  instances for `Const` and `Constant`
* Add `afoldVL` and `iafoldVL` constructors
* Rename `toAtraversalVL` to `atraverseOf`, and `toIxAtraversalVL` to `iatraverseOf`
* Generalise `element` and `elementOf` to construct `IxAffineTraversal`s
  instead of `IxTraversal`s
* Change `mapping` to work on optic kinds other than `Iso`: it now supports
  `Lens` and `Prism` degenerating to `Getter` and `Review` respectively
* Generalise `ignored` to be an `IxAffineTraversal` instead of an `IxTraversal`
* Add `singular` and `isingular`
* Add `(^?!)` operator
* Expose `Curry` and `CurryCompose`
* Show expected elimination forms on optic kind mismatch
* Use stricter `uncurry'` for better performance
* Add hidden `LabelOptic` instance to postpone instance resolution
* Add `GeneralLabelOptic` for pluggable generic optics as labels
* Document monoidal structures of `Fold`s
* Remove proxy argument from `implies`
* Add `itoList`

# optics-core-0.2 (2019-10-18)
* Add `non`, `non'` and `anon` to `Optics.Iso`
* `ix` can produce optic kinds other than `AffineTraversal`
* Generalise type of `generic1`
* Move some internal definitions out to new `indexed-profunctors` package
* Introduce `OpticKind` and `IxList` type synonyms for better type inference
* Make `itraverse` for `Seq` faster for `containers >= 0.6.0`
* Assorted documentation improvements

# optics-core-0.1 (2019-09-02)
* Initial release
