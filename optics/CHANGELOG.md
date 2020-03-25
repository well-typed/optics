# optics-0.3 (TBD)
* GHC-8.10 support
* Add `filteredBy` and `unsafeFilteredBy`
* Add indexed instances for `Const` and `Constant`
* Add `afoldVL` and `iafoldVL` constructors
* Rename `toAtraversalVL` to `atraverseOf`, and `toIxAtraversalVL` to `iatraverseOf`
* `element` and `elementOf` construct `(Ix)AffineTraversal`
* Change `mapping` to work also on `Lens` and `Prism` degenerating to `Getter` and `Review`.
* Generalise `ignored` to be an `IxAffineTraversal` instead of an `IxTraversal`
* Add `singular` and `isingular`
* Add `(^?!)` operator
* Expose `Curry` and `CurryCompose`
* Show expected elimination forms on optic kind mismatch
* Use stricter uncurry for better performance
* Add GeneralLabelOptic for pluggable generic optics as labels
* Document monoidal structures of Folds
* Remove proxy argument from implies

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
