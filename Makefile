# We use make for small scripts

all :
	cabal new-build all

doctest :
	cd optics-core && doctest -XDataKinds -XFlexibleContexts -XGADTs -XRankNTypes -XScopedTypeVariables -XTupleSections -XTypeFamilies src

ghcid-optics-core :
	ghcid -c 'cabal new-repl optics-core'
