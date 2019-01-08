# We use make for small scripts

all :
	cabal new-build all

haddock :
	cabal new-haddock optics-core

# Build with all supported GHCs, run tests.
validate : all doctest
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

