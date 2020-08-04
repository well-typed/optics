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
