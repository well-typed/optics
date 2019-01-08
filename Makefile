# We use make for small scripts

all :
	cabal new-build all

doctest :
	cd optics-core && doctest -XBangPatterns -XDefaultSignatures -XFlexibleContexts -XFlexibleInstances -XFunctionalDependencies -XDeriveFunctor -XGADTs -XLambdaCase -XMultiParamTypeClasses -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies -XTypeOperators src

ghcid-optics-core :
	ghcid -c 'cabal new-repl optics-core'
