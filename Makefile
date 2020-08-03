# We use make for small scripts

.PHONY : all build test haddock validate doctest diagrams

all : build

build :
	cabal new-build all

test : build
	cabal new-run optics-tests

haddock :
	cabal new-haddock all

# Build with all supported GHCs, run tests.
validate : build doctest
	cabal new-build all --builddir=dist-validate-8.0.2 -w ghc-8.0.2 --write-ghc-environment-files=never
	cabal new-build all --builddir=dist-validate-8.2.2 -w ghc-8.2.2 --write-ghc-environment-files=never
	cabal new-build all --builddir=dist-validate-8.4.4 -w ghc-8.4.4 --write-ghc-environment-files=never
	cabal new-build all --builddir=dist-validate-8.6.3 -w ghc-8.6.3 --write-ghc-environment-files=never

	cabal new-run optics-tests --builddir=dist-validate-8.0.2 -w ghc-8.0.2 --write-ghc-environment-files=never
	cabal new-run optics-tests --builddir=dist-validate-8.2.2 -w ghc-8.2.2 --write-ghc-environment-files=never
	cabal new-run optics-tests --builddir=dist-validate-8.4.4 -w ghc-8.4.4 --write-ghc-environment-files=never
	cabal new-run optics-tests --builddir=dist-validate-8.6.3 -w ghc-8.6.3 --write-ghc-environment-files=never

doctest : build
	doctest --fast $$(find generic-optics/src  optics/src optics-core/src optics-extra/src optics-sop/src optics-th/src optics-vl/src -name '*.hs') -XBangPatterns -XDefaultSignatures -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XDeriveFunctor -XGADTs -XLambdaCase -XMultiParamTypeClasses -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies -XTypeOperators -XDataKinds -XTypeApplications -XInstanceSigs

doctest-optics-core : build
	doctest --fast indexed-profunctors/src optics-core/src -XBangPatterns -XDefaultSignatures -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XDeriveFunctor -XGADTs -XLambdaCase -XMultiParamTypeClasses -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies -XTypeOperators -XDataKinds -XTypeApplications -XInstanceSigs

doctest-optics-extra : build
	doctest --fast optics-extra/src -XBangPatterns -XDefaultSignatures -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XDeriveFunctor -XGADTs -XLambdaCase -XMultiParamTypeClasses -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies -XTypeOperators -XDataKinds -XTypeApplications -XInstanceSigs

ghcid-optics-core :
	ghcid -c 'cabal new-repl optics-core'

codegen-subtypes :
	cabal new-run --builddir=dist-codegen --project-file=cabal.codegen.project optics-codegen-subtypes -- subtypes

codegen-join :
	cabal new-run --builddir=dist-codegen --project-file=cabal.codegen.project optics-codegen-subtypes -- join

DIAGRAMS=optics reoptics indexedoptics

diagrams : $(DIAGRAMS:%=optics/diagrams/%.png) optics-core/diagrams/reoptics.png

OPTIC_KINDS=AffineFold AffineTraversal Fold Getter Iso Lens Prism ReversedLens ReversedPrism Review Setter Traversal

per-kind-diagrams : $(OPTIC_KINDS:%=optics-core/diagrams/%.png)

metametapost/%.mp : metametapost/src/MetaMetaPost.hs metametapost/src/Cli.hs
	cabal new-build metametapost-optics
	$$(cabal new-exec which metametapost-optics) $* > $@

metametapost/%.png : metametapost/%.mp
	make -C metametapost $*.png

# This rule was:
# cp $< $@
optics/diagrams/%.png : metametapost/%.png
	optipng -o7 -zm1-9 $< -dir optics/diagrams/

optics-core/diagrams/%.png : metametapost/%.png
	optipng -o7 -zm1-9 $< -dir optics-core/diagrams/
