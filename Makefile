# We use make for small scripts

.PHONY : all build test haddock validate doctest diagrams

all : build

build :
	cabal new-build all

test : build
	cabal new-run optics-tests

haddock :
	cabal new-haddock optics-core

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

doctest :
	cd optics-core &&  doctest  -XBangPatterns -XDefaultSignatures -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XDeriveFunctor -XGADTs -XLambdaCase -XMultiParamTypeClasses -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies -XTypeOperators -XDataKinds src

ghcid-optics-core :
	ghcid -c 'cabal new-repl optics-core'

codegen-subtypes :
	cabal new-run --builddir=dist-codegen --project-file=cabal.codegen.project optics-codegen-subtypes -- subtypes

codegen-join :
	cabal new-run --builddir=dist-codegen --project-file=cabal.codegen.project optics-codegen-subtypes -- join

diagrams : optics-core/optics.png optics-core/reoptics.png optics-core/indexedoptics.png

metametapost/optics.mp metametapost/reoptics.mp metametapost/indexedoptics.mp : build metametapost/src/MetaMetaPost.hs
	cabal new-build metametapost-optics
	$$(cabal new-exec which metametapost-optics) hierarchy > metametapost/optics.mp
	$$(cabal new-exec which metametapost-optics) reoptics > metametapost/reoptics.mp
	$$(cabal new-exec which metametapost-optics) indexedoptics > metametapost/indexedoptics.mp

metametapost/optics.png metametapost/reoptics.png metametapost/indexedoptics.png : metametapost/optics.mp metametapost/reoptics.mp metametapost/indexedoptics.mp
	make -C metametapost

optics-core/optics.png : metametapost/optics.mp
	cp metametapost/optics.png optics-core/optics.png

optics-core/reoptics.png : metametapost/reoptics.mp
	cp metametapost/reoptics.png optics-core/reoptics.png

optics-core/indexedoptics.png : metametapost/indexedoptics.mp
	cp metametapost/indexedoptics.png optics-core/indexedoptics.png
