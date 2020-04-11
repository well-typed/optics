# optics-0.3 (2020-04-15)
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
* Use stricter `uncurry` for better performance
* Add hidden `LabelOptic` instance to postpone instance resolution
* Add `GeneralLabelOptic` for pluggable generic optics as labels
* Document monoidal structures of `Fold`s
* Remove proxy argument from `implies`
* Add `itoList`
* Improvements to TH-generated optics:
  - `LabelOptic` instances make optic kind a type equality for better type inference
  - `LabelOptic` instances for field optics work properly in the presence of type families
  - Fixed calculation of phantom types in `LabelOptic` prism instances
  - Better support for generating optics in the presence of kind polymorphism

# optics-0.2 (2019-10-18)
* Add `non`, `non'` and `anon` to `Optics.Iso`
* `ix` can produce optic kinds other than `AffineTraversal`
* Generalise type of `generic1`
* Move `use` from `Optics.View` to `Optics.State` and restrict its type
* Add `preuse` to `Optics.State`
* Rename `use`, `uses`, `listening` and `listenings` to reflect the fact that
  they have `ViewResult`-generalised types
* Add `noPrefixFieldLabels` and `noPrefixNamer` to `Optics.TH`
* Move some internal definitions out to new `indexed-profunctors` package
* Introduce `OpticKind` and `IxList` type synonyms for better type inference
* Make `itraverse` for `Seq` faster for `containers >= 0.6.0`
* Assorted documentation improvements

# optics-0.1 (2019-09-02)
* Initial release
