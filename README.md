# optics

[![Build Status](https://travis-ci.org/well-typed/optics.svg?branch=master)](https://travis-ci.org/well-typed/optics)
[![Hackage](https://img.shields.io/hackage/v/optics.svg)](https://hackage.haskell.org/package/optics)

The [`optics`](https://hackage.haskell.org/package/optics) family of Haskell
packages make it possible to define and use Lenses, Traversals, Prisms and other
*optics*, using an abstract interface. They are roughly comparable in
functionality with the [`lens`](https://hackage.haskell.org/package/lens)
package, but explore a different part of the design space. For a detailed
introduction, see the [Haddocks for the main `Optics`
module](https://hackage.haskell.org/package/optics/docs/Optics.html).


## Authors and contributors

The authors of the `optics` family of packages are:

 * Adam Gundry
 * Andres Löh
 * Andrzej Rybczak
 * Oleg Grenrus

Our thanks go to those who have (involuntarily) contributed code and ideas to
`optics`. In particular, we have liberally reused parts of the `lens` package by
Edward Kmett and contributors.


## Package structure

### Officially supported packages

 * [`optics`](https://hackage.haskell.org/package/optics) is a
   "batteries-included" package with many dependencies. It incorporates:

   * [`optics-core`](https://hackage.haskell.org/package/optics-core): core
     definitions with a minimal dependency footprint.

   * [`optics-extra`](https://hackage.haskell.org/package/optics-extra): extra
     definitions and instances that extend `optics-core`, incurring dependencies
     on various boot library packages.

   * [`optics-th`](https://hackage.haskell.org/package/optics-th): machinery to
     construct optics using `TemplateHaskell`.

   * [`indexed-profunctors`](https://hackage.haskell.org/package/indexed-profunctors):
     internal definitions of indexed profunctor representation.

 * [`optics-vl`](https://hackage.haskell.org/package/optics-vl): utilities for
   compatibility with van Laarhoven isomorphisms and prisms, as defined in the
   `lens` library.  This package is not included in `optics` as it imposes a
   dependency on `profunctors`.  Note that `optics-core` already supports
   conversion for van Laarhoven lenses and various other optics.

### Work in progress packages

These packages have not (yet) been officially released. If you find them
useful, we would welcome offers to maintain these packages.

 * `optics-sop`: generic construction of optics using the `generics-sop`
   package, and optics for `generics-sop` types.

 * `template-haskell-optics`: optics for working with types in the
   `template-haskell` package (see `optics-th` for *using* `TemplateHaskell` to
   construct optics).

### Internal packages

These packages are for internal use only, and are not intended to be released:

 * `metametapost`: generates diagrams used in the documentation, and an example
   of using `optics`.

 * `optics-codegen`: code generator for the `Is` class and `Join` type family
   used internally by `optics`.
