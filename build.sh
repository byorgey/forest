#!/bin/sh
mkdir -p _shake
mkdir -p _blocks
ghc --make Shake.hs -package shake -package text-2.1.2 -package mtl -package hashable-1.5.1.0 -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"
