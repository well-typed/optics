# optics-th-0.3 (2020-04-15)
* `optics-core-0.3` compatible release
* GHC-8.10 support
* Improvements to TH-generated optics:
  - `LabelOptic` instances make optic kind a type equality for better type inference
  - `LabelOptic` instances for field optics work properly in the presence of type families
  - Fixed calculation of phantom types in `LabelOptic` prism instances
  - Better support for generating optics in the presence of kind polymorphism

# optics-th-0.2 (2019-10-18)
* Add `noPrefixFieldLabels` and `noPrefixNamer` to `Optics.TH`

# optics-th-0.1 (2019-09-02)
* Initial release
