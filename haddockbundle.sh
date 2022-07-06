#!/bin/sh

# First we build haddocks for each package
# We build them separately, as apparently cabal haddock doesn't link
# to other /local/ packages.
#
# Therefore you need first to upload packages to Hackage before docs
# can be built.
#
# NOTE: actually this doesn't work either, but it was a good try.

# Same GHC as hackage doc builder (apparently) uses
GHC=ghc-8.10.7

TOPDIR=$(pwd)
PACKAGES="optics optics-core optics-th optics-extra"

for PKG in $PACKAGES; do
	echo "$PKG"
	cd "$PKG" || exit

	pwd

	# We create a cabal.project, so the root project is not used.
	# Apparently cabal haddock --ignore-project doesn't ignore project after all.
	echo 'packages: .' > cabal.project

	cabal haddock --haddock-for-hackage --with-compiler "$GHC"
	cp dist-newstyle/*-docs.tar.gz "$TOPDIR/dist-newstyle/"

	rm -f cabal.project

	cd "$TOPDIR" || exit
done


# Create haddock bundle
cabal run --project-file=cabal.haddockbundle.project optics-haddockbundle
