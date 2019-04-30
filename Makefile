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

# You need patched doctest which knows --no-interpret
doctest : build
	doctest --no-interpret --fast $$(find optics/src -name '*.hs')
	doctest --no-interpret --fast $$(find generic-optics/src -name '*.hs')
	doctest --no-interpret --fast $$(find optics-core/src -name '*.hs') -XBangPatterns -XDefaultSignatures -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XDeriveFunctor -XGADTs -XLambdaCase -XMultiParamTypeClasses -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies -XTypeOperators -XDataKinds -XTypeApplications -XInstanceSigs

ghcid-optics-core :
	ghcid -c 'cabal new-repl optics-core'

codegen-subtypes :
	cabal new-run --builddir=dist-codegen --project-file=cabal.codegen.project optics-codegen-subtypes -- subtypes

codegen-join :
	cabal new-run --builddir=dist-codegen --project-file=cabal.codegen.project optics-codegen-subtypes -- join

DIAGRAMS=optics reoptics indexedoptics

diagrams : optics/optics.png optics/reoptics.png optics/indexedoptics.png

metametapost/%.mp : metametapost/src/MetaMetaPost.hs metametapost/src/Cli.hs
	cabal new-build metametapost-optics
	$$(cabal new-exec which metametapost-optics) $* > $@

metametapost/%.png : metametapost/%.mp
	make -C metametapost $*.png

optics/%.png : metametapost/%.png
	cp $< $@
