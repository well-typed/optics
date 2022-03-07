#!/bin/sh
#
# For this to work you need to:
#
# - Put "write-ghc-environment-files: always" in your cabal.project.local.
#
# - Compile doctest with the same GHC version the project currently uses.
#

set -eu

run_doctest() {
  pushd "${1}"
  doctest \
    "${2}" \
    -XHaskell2010 \
    -XBangPatterns \
    -XConstraintKinds \
    -XDefaultSignatures \
    -XDeriveFoldable \
    -XDeriveFunctor \
    -XDeriveGeneric \
    -XDeriveTraversable \
    -XEmptyCase \
    -XFlexibleContexts \
    -XFlexibleInstances \
    -XFunctionalDependencies \
    -XGADTs \
    -XGeneralizedNewtypeDeriving \
    -XInstanceSigs \
    -XKindSignatures \
    -XLambdaCase \
    -XOverloadedLabels \
    -XPatternSynonyms \
    -XRankNTypes \
    -XScopedTypeVariables \
    -XTupleSections \
    -XTypeApplications \
    -XTypeFamilies \
    -XTypeOperators \
    -XViewPatterns
  popd
}

if [ $# -eq 0 ]; then
    dirs="optics-core optics-extra optics-th optics"
else
    dirs="$@"
fi

for dir in $dirs; do
    run_doctest "$dir" src
done
