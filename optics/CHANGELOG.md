# optics-0.2 (????-??-??)
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
