# optics-0.4.2.1 (2023-06-22)
* Fix compilation of tests with GHC 9.4 and 9.6

# optics-0.4.2 (2022-05-19)
* Allow `transformers-0.6` and `mtl-2.3`

  Note that `optics-extra` no longer defines `Zoom` instances for `ErrorT` or `ListT` when
  building with `mtl-2.3` or later. This is because `MonadState` is a superclass of
  `Zoom`, and the `MonadState` instances for `ErrorT` and `ListT` were removed in
  `mtl-2.3`. Be watchful of this if you build `optics-extra` with `mtl-2.3` (or
  later) combined with an older version of `transformers` (pre-0.6) that defines
  `ErrorT` or `ListT`.  Similarly for `Magnify` and `MagnifyMany`.

# optics-0.4.1 (2022-03-22)
* Add support for GHC-9.2
* Add `is` ([#410](https://github.com/well-typed/optics/pull/410))
* Improve error messages related to the `JoinKinds` class
  ([#439](https://github.com/well-typed/optics/pull/439))
* Port `universeOf`, `cosmosOf`, `paraOf`, `rewriteOf`, `transformOf`,
  `rewriteMOf` and `transformMOf` from `Control.Lens.Plated`
  ([#379](https://github.com/well-typed/optics/pull/379))
* Add `(%?)` composition operator
  ([#434](https://github.com/well-typed/optics/pull/434))

# optics-0.4 (2021-02-22)
* See [migration-guide-0.4.md](https://github.com/well-typed/optics/blob/master/migration-guide-0.4.md) for more details
* Add support for GHC-9.0
* Drop support for GHC-8.0
* The `FunctorWithIndex`, `FoldableWithIndex` and `TraversableWithIndex` type
  classes have been migrated to a new package,
  [`indexed-traversable`](https://hackage.haskell.org/package/indexed-traversable)
  ([#370](https://github.com/well-typed/optics/pull/370))
* Add `adjoin`, `iadjoin` and `both` to `Optics.[Ix]Traversal`
  ([#332](https://github.com/well-typed/optics/pull/332),
   [#372](https://github.com/well-typed/optics/pull/372))
* Add `ifst` and `isnd` to `Optics.IxLens`
  ([#389](https://github.com/well-typed/optics/pull/389))
* Generalize types of `generic`
  ([#376](https://github.com/well-typed/optics/pull/376))
* Make `chosen` an indexed lens to see which value is traversed
  ([#335](https://github.com/well-typed/optics/pull/335))
* Remove `GeneralLabelOptic` extensibility mechanism
  ([#361](https://github.com/well-typed/optics/pull/361))
* Add `gfield`, `gafield`, `gconstructor`, `gposition` and `gplate` for
  generics-based data access
  ([#358](https://github.com/well-typed/optics/pull/358),
   [#361](https://github.com/well-typed/optics/pull/361))
* Add support for generics-based field lenses and constructor prisms (`gfield`
  and `gconstructor`) to `LabelOptic` so they can be used via `OverloadedLabels`
  ([#361](https://github.com/well-typed/optics/pull/361))
* Remove unnecessary INLINE pragmas to reduce compile times
  ([#394](https://github.com/well-typed/optics/pull/394))
* Simplify the type of `(%)` using new `JoinKinds` and `AppendIndices` classes
  in place of the `Join` and `Append` type families
  ([#397](https://github.com/well-typed/optics/pull/397),
   [#399](https://github.com/well-typed/optics/pull/399))
* Print missing language extensions during TH generation of labels if there are
  any ([#352](https://github.com/well-typed/optics/pull/352))
* Add support for getters of rank1 polymorphic fields to optics generated with
  the `makeFieldLabels` family of functions
  ([#365](https://github.com/well-typed/optics/pull/365))
* Extend support of type-changing optics generated with the `makeFieldLabels`
  family to type parameters that are phantom and applied to non-injective type
  families
  ([#365](https://github.com/well-typed/optics/pull/365))
* Fix TH generation of optics for poly-kinded data families
  ([#378](https://github.com/well-typed/optics/pull/378))
* Fix `declareFieldLabels` when a field type refers to a type defined in the
  same quote
  ([#380](https://github.com/well-typed/optics/pull/380))

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
